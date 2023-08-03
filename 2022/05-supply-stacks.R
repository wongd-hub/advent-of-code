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


## 05a Part 1 ----
### Data ingestion ----

#' Raw text file - use empty line to delimit the crate drawing and the instructions.
day_5_input_fp <- max(list.files(file.path('2022', 'inputs'), 'day_5', full.names = TRUE))

#'  Which line is empty / is the divider?
crates_raw <- readLines(day_5_input_fp)
divider_idx <- which(crates_raw == '')

#'  => crate drawings
crate_drawings <- crates_raw[1:(divider_idx - 1)] # readLines(day_5_input_fp, divider_idx - 1)

#'  => crate movements
crate_movements <- crates_raw[(divider_idx+1):length(crates_raw)]

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
#' @param edit_global A string with the name of the global object to replace
#'   with the resulting list. `NULL` if you don't want to edit a global object
execute_instruction <- function(instruction, crate_list, edit_global = NULL) {

  #' Move [num_crates] crates from the top of [stack_from] to the top of
  #' [stack_to]
  for (i in 1:instruction$num_crates) {
    
    #' Shift crate off the top of [stack_from]
    moving_crate <- crate_list[[instruction$stack_from]][[1]]
    
    #' If resulting [stack_from] will be empty after move, replace with NA
    if (length(crate_list[[instruction$stack_from]]) == 1) {
      
      crate_list[[instruction$stack_from]] <- NA_character_
      
    } else {
      
      crate_list[[instruction$stack_from]] <- crate_list[[instruction$stack_from]][2:length(crate_list[[instruction$stack_from]])]
      
    }
    
    if (is.na(moving_crate)) {
      cat('Error - crate', i, 'does not exist in stack', instruction$stack_from)
      stop()
    }
    
    #' Put moving crate on the top of [stack_to] - if [stack_to] is empty then replace the NA with it
    if (length(crate_list[[instruction$stack_to]]) == 1 & is.na(crate_list[[instruction$stack_to]][[1]])) {
      
      crate_list[[instruction$stack_to]] <- moving_crate
      
    } else {
      
      crate_list[[instruction$stack_to]] <- c(moving_crate, crate_list[[instruction$stack_to]])
      
    }
    
  }
  
  if (!is.null(edit_global)) assign('edit_global', crate_list, envir = .GlobalEnv) else return(crate_list)
  
}

#' Testing - assigning global doesn't seem to work yet, lets just do this in a global for-loop
execute_instruction(extract_instruction_nums('move 1 from 1 to 2', return_vector = T), crate_stacks)
execute_instruction(extract_instruction_nums('move 2 from 7 to 2', return_vector = T), crate_stacks)

if (F) {
  
  # Would assigning to separate vars for readability make this much slower?
  execute_instruction_readable <- function(instruction, crate_list, edit_global = NULL) {
    
    #' Explicitly assign elements of the vector to their own vars, otherwise square bracket hell :)
    stack_from <- instruction[['stack_from']]
    stack_to   <- instruction[['stack_to']]
    num_crates <- instruction[['num_crates']]
    
    #' Move [num_crates] crates from the top of [stack_from] to the top of
    #' [stack_to]
    for (i in 1:num_crates) {
      
      #' Shift crate off the top of [stack_from]
      moving_crate <- crate_list[[stack_from]][[1]]
      crate_list[[stack_from]] <- crate_list[[stack_from]][2:length(crate_list[[stack_from]])]
      
      #' Put moving crate on the top of [stack_to]
      crate_list[[stack_to]] <- c(moving_crate, crate_list[[stack_to]])
      
    }
    
    if (!is.null(edit_global)) assign('edit_global', crate_list, envir = .GlobalEnv) else return(crate_list)
    
  }
  
  
  microbenchmark(
    no_extra_assignment = execute_instruction(extract_instruction_nums('move 2 from 7 to 2', return_vector = T), crate_stacks),
    extra_assignment    = execute_instruction_readable(extract_instruction_nums('move 2 from 7 to 2', return_vector = T), crate_stacks)
  )
  
  #> Unit: microseconds
  #>                expr    min      lq     mean  median      uq     max neval
  #> no_extra_assignment 96.793 97.8965 100.0982 98.6045 100.021 114.001   100
  #> extra_assignment    95.667 96.7925 102.3545 97.5630  98.772 305.459   100
  
  # A little slower, for now, leave as it is
  
}

### Executing instructions ----

crate_stacks_og <- crate_stacks

#' If needed to reset
if (F) crate_stacks <- crate_stacks_og

for (i in 1:length(crate_movements)) {
  
  cat('Executing:', crate_movements[[i]], '\n')
  
  instruction_values <- extract_instruction_nums(crate_movements[[i]], return_vector = T)
  
  crate_stacks <- execute_instruction(instruction_values, crate_stacks)
  #' There are NAs in the stacks now?? still NAs even with stop() built in. Go line by line to see what happens
}


crate_stacks %>% sapply(first) %>% paste(collapse = '')
