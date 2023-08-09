# 05 Supply Stacks ----
#' https://adventofcode.com/2022/day/5

#' The elves can't get started on their expedition until all required crates
#' have been unloaded from the ship. The crates are currently in stacks, so we
#' need to shuffle crates around so that we can expose the required crates at
#' the top of each pile.
#' 
#' - Provided first with a drawing of what each stack looks like 
#' - Also provided a list of moves the crane operator has planned out
#' 
#'   e.g. 'move 2 from 2 to 1'
#'        two crates moved [one at a time] from stack 2 to stack 1
#' 
#' Which crates will be at the top of each pile at the end of this list of
#' moves? These will be the required crates.
#'
#' Can't really vectorise here since the instructions must be executed
#' sequentially.

source(file.path('2022', 'utils', 'libs_and_funs.R'))

## 05a Part 1 ----
### Data ingestion ----

#'  Which line is empty / is the divider?
crates_raw <- get_input(
  2022, 5,
  read_fn = function(fp) {
    
    raw_input <- readLines(fp)
    
    #' An empty line divides the crate drawing and the crate movement
    #' instructions
    divider_idx <- which(raw_input == '')
    
    list(
      #' Crate drawings are above the divider
      crate_drawings = raw_input[1:(divider_idx - 1)],
      #' Crate movement instructions are below
      crate_movements = raw_input[(divider_idx + 1):length(raw_input)]
    )
    
  }
)

crate_drawings <- crates_raw$crate_drawings
crate_movements <- crates_raw$crate_movements

### Crate Drawing Manipulation ----

#' Manipulate crate drawings. We want to take it from this:
#' 
# [D]        
# [N] [C]    
# [Z] [M] [P]
#  1   2   3
#' 
#' To:
#' 
# list(
#   c('D', 'N', 'Z'),
#   c('C', 'M),
#   c('P')
# )
#' 
#' Which will make it easier to manipulate these stacks. Prefer the top to be at
#' the first index since we'll be manipulating from the top mainly. Will save us
#' some length(vec) calls.

#' First, get index of each stack number; then use that to pull out numbers from
#' each level/string.
deconstructed_crate_numbers <- str_split_1(crate_drawings[length(crate_drawings)], '')
crate_num_idx <- which(str_detect(deconstructed_crate_numbers, '[0-9]'))

#' Remove crate numbers
crate_stacks <- crate_drawings[1:(length(crate_drawings) - 1)] %>% 
  #' Deconstruct strings - each character is now an element in a vector
  str_split('') %>% 
  #' Across each character vector, pull only positions where crate identifiers
  #' will sit
  map(~{.x[crate_num_idx]}) %>% 
  #' Transpose list so that each list item now represents a stack of crates
  transpose() %>% 
  #' Remove empty characters
  map(~{unlist(.x) %>% Filter(\(x) x != " ", .)})

#' This now represents the crate stacks properly and looks like:
#> [[1]]
#> [1] "D" "Z" "T" "H"
#> 
#> [[2]]
#> [1] "S" "C" "G" "T" "W" "R" "Q"

### Manipulation and application of crate movements ----

#' All instructions come in the form:
#'   move [num_crates (one by one)] from [stack_from] to [stack_to]
#' Let's encapsulate this in a function so that we can apply each instruction
#' one by one

#' Extract required info from instruction
#'
#' Takes an instruction of the form "move [num_crates (one by one)] from
#' [stack_from] to [stack_to]" and outputs a named vector containing the three
#' relevant pieces of information
#'
#' @param str_instruction An instruction string/vector of the form shown above
#' @param return_vector A boolean - whether to return a vector, not a list, if
#'   only one instruction is provided
#'   
#' @examples 
#' extract_instruction_nums(crate_movements[1:3])
extract_instruction_nums <- function(str_instruction, return_single = F) {
  
  instructions <- str_instruction %>% 
    str_extract_all('\\d+') %>% 
    map(~{
      .x %>% 
        map(as.numeric) %>% 
        set_names(c('num_crates', 'stack_from', 'stack_to'))
    })
  
  if (any(instructions %>% sapply(length) > 3)) stop('More than 3 numbers found in an instruction')
  if (length(instructions) == 1 & return_single) instructions[[1]] else instructions
  
}

#' Execute instruction
#'
#' Given a vector of instruction values, perform required movements to the
#' provided crate stack list object (in-place(?)).
#'
#' @param instruction A single named vector with `num_crates`, `stack_from`,
#'   `stack_to` values present
#' @param crate_list A list containing crate stacks as vectors; with the top
#'   crate as the first element, and the bottom crate as the last
execute_instruction <- function(instruction, crate_list) {

  #' Move [num_crates] crates from the top of [stack_from] to the top of
  #' [stack_to]. Perform in a for-loop since we need to move crates one at a time.
  for (i in 1:instruction$num_crates) {
    
    #' Shift crate off the top of [stack_from]
    moving_crate <- crate_list[[instruction$stack_from]][[1]]
    
    #' If resulting [stack_from] will be empty after move, replace with NA
    crate_list[[instruction$stack_from]] <- if (length(crate_list[[instruction$stack_from]]) == 1) {
      NA_character_
    } else {
      crate_list[[instruction$stack_from]][2:length(crate_list[[instruction$stack_from]])]
    }
    
    if (is.na(moving_crate)) {
      cat('Error - crate', i, 'does not exist in stack', instruction$stack_from)
      stop()
    }
    
    #' Put moving crate on the top of [stack_to] - if [stack_to] is empty then replace the NA with it
    crate_list[[instruction$stack_to]] <- if (
      length(crate_list[[instruction$stack_to]]) == 1 & 
        is.na(crate_list[[instruction$stack_to]][[1]])
    ) moving_crate else c(moving_crate, crate_list[[instruction$stack_to]])
    
  }
  
  crate_list
  
}

#' Testing
execute_instruction(extract_instruction_nums('move 1 from 1 to 2', return_single = T), crate_stacks)
execute_instruction(extract_instruction_nums('move 2 from 7 to 2', return_single = T), crate_stacks)


### Executing instructions ----

#' Run all instructions
#'
#' Takes a vector of string representations of the instructions, converts those
#' to machine-readable, then executes them in sequence.
#'
#' @param instructions A character vector containing the instructions, e.g.
#'   c('move 1 from 3 to 7', 'move 1 from 3 to 2')
#' @param crate_pattern_list A list containing vectors, each representing a
#'   crate stack
#' @param verbose A boolean representing whether to print logs to console
run_all_instructions <- function(instructions, crate_pattern_list, execution_function = execute_instruction, verbose = F) {
  
  instruction_list <- extract_instruction_nums(instructions)
  crate_list <- crate_pattern_list
  
  for (i in 1:length(instruction_list)) {
    
    if (verbose) cat('Executing:', instructions[[i]], '\n')
    
    crate_list <- execution_function(instruction_list[[i]], crate_list)
    
  }
  
  crate_list
  
}

#' Get final state of crate stacks
final_crate_stacks <- run_all_instructions(crate_movements, crate_stacks, verbose = T)

#' Check for any NAs that might have snuck through
final_crate_stacks %>% sapply(\(x) sum(is.na(x))) %>% sum() == 0 # TRUE

#' Get top crate of each stack
final_crate_stacks %>% sapply(first) %>% paste(collapse = '')

#' Answer is [RFFFWBPNS]

## 05b Part 2 ----

#' It turns out the crane the elf is using is a different model which can pick
#' up multiple crates at once.

#' Execute instruction (new crane)
#'
#' This crane can pick up and move more than 1 crate at a time (unlimited number
#' of crates at a time).
#'
#' @param instruction A single named vector with `num_crates`, `stack_from`,
#'   `stack_to` values present
#' @param crate_list A list containing crate stacks as vectors; with the top
#'   crate as the first element, and the bottom crate as the last
execute_instruction_new_crane <- function(instruction, crate_list) {
  
  #' Move [num_crates] crates from the top of [stack_from] to the top of
  #' [stack_to] simultaneously
  
  #' Since we're potentially taking multiple crates off [stack_from], make sure
  #' we have enough in there first
  if (length(crate_list[[instruction$stack_from]]) < instruction$num_crates) {
    cat('Not enough crates in stack', instruction$stack_from)
    stop()
  }
  
  #' Shift crate/s off the top of [stack_from]
  moving_crates <- crate_list[[instruction$stack_from]][1:instruction$num_crates]
  
  #' If resulting [stack_from] will be empty after move, replace with NA
  crate_list[[instruction$stack_from]] <- if (length(crate_list[[instruction$stack_from]]) == instruction$num_crates) {
    NA_character_
  } else {
    crate_list[[instruction$stack_from]][(instruction$num_crates+1):length(crate_list[[instruction$stack_from]])]
  }
  
  if (any(is.na(moving_crates))) {
    cat('Error - one of the moving crates does not exist in stack', instruction$stack_from)
    stop()
  }
  
  #' Put moving crates on the top of [stack_to] - if [stack_to] is empty then replace the NA with it
  crate_list[[instruction$stack_to]] <- if (
    length(crate_list[[instruction$stack_to]]) == 1 & 
    is.na(crate_list[[instruction$stack_to]][[1]])
  ) moving_crates else c(moving_crates, crate_list[[instruction$stack_to]])
  
  crate_list
  
}

#' Testing
execute_instruction_new_crane(extract_instruction_nums('move 4 from 1 to 2', return_single = T), crate_stacks)

#' Sub in new execution function
final_crate_stacks_new_crane <- run_all_instructions(crate_movements, crate_stacks, execute_instruction_new_crane, verbose = T)

#' Check for any NAs that might have snuck through
final_crate_stacks_new_crane %>% sapply(\(x) sum(is.na(x))) %>% sum() == 0 # TRUE

#' Get top crate of each stack
final_crate_stacks_new_crane %>% sapply(first) %>% paste(collapse = '')

#' Answer is [CQQBBJFCS]