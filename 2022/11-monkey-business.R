# 11 Monkey in the Middle ----

#' Monkeys have decided to throw your belongings around. You notice that how the
#' monkeys decide who to throw to next is based on your worry level for each
#' item.
#' 
#' A single monkey's instruction looks like this:
#' 
#' Monkey I:
#'  Starting items: X, Y
#'  Operation: new = old * 19
#'  Test: divisible by 23
#'    If true: throw to monkey 2
#'    If false: throw to monkey 3
#'    
#' Starting items lists what items the monkey will inspect in order. Operation
#' determines how your worry level changes when the monkey inspects the item.
#' Test provides the decision rule which determines who the monkey throws to.
#' 
#' After a monkey inspects an item, but before it assesses your worry level, you
#' are relieved the monkey didn't break the item and your worry divides by 3 and
#' rounds down to the nearest integer.
#' 
#' When an item is throw to a monkey, it enters the back of their queue to be
#' inspected upon the next round, keeping all worry transforms that have
#' occurred to it until then.
#' 
#' A monkey's turn goes like this for each item it holds, in order
#'  - Monkey inspects item with worry level X
#'  - Your worry is multiplied by 19 (X * 19)
#'  - You are relieved they didn't break the item, your worry divides by 3 (floor((X * 19) / 3))
#'  - Current worry is not divisible by 23, throw to monkey 3
#'  
#' A full round ends once each monkey has taken their turn. Then the round
#' starts again with each monkey's updated queue. After 20 rounds, which 2
#' monkeys inspected the most items? Calculate the monkey business (throws x
#' throws) for these two monkeys.

source(file.path('2022', 'utils', 'libs_and_funs.R'))

## 11a Part 1 ----
### Data ingestion ----

#' Raw text file
monkey_business <- get_input(2022, 11)


### Function definitions ----

#' Pull initial monkey details
#'
#' Parse raw monkey information file into something we can use
#'
#' @param x Vector representing raw monkey data
get_monkey_details <- function(x) {
  
  x_noempty <- x %>% 
    str_subset('^$', negate = T) %>% 
    str_trim()
  
  mky_names <- str_subset(x_noempty, '^Monkey \\d+:') %>% 
    str_extract('\\d+')
  
  mky_info_start_idx <- which(str_detect(x_noempty, '^Monkey \\d+:'))
  mky_info_end_idx   <- c((mky_info_start_idx - 1) %>% .[2:length(.)], length(x_noempty))
  
  tibble(
    mky_start_idx = mky_info_start_idx,
    mky_end_idx   = mky_info_end_idx
  ) %>% 
    pmap(function(mky_start_idx, mky_end_idx) {
      
      # Pull subset
      mky_subset <- x_noempty[mky_start_idx:mky_end_idx]
      
      # Pull starting items
      starting_items <- mky_subset[[2]] %>% 
        str_remove('^Starting items: ') %>% 
        str_split(pattern = ', ') %>% .[[1]] %>% 
        as.numeric()
      
      # Operation
      operation <- mky_subset[[3]] %>% str_remove('^Operation: new = ')
      
      # Test
      division_test <- mky_subset[[4]] %>% str_remove('^Test: divisible by ') %>% as.numeric() # All tests are dividing by something
      
      # If true
      true <- mky_subset[[5]] %>% str_remove('^If true: throw to monkey ')
      
      # If false
      false <- mky_subset[[6]] %>% str_remove('^If false: throw to monkey ')
      
      results <- list(
        items          = starting_items,
        operation      = operation,
        division_test  = division_test,
        true           = true,
        false          = false
      )
      
      
    }) %>% 
    set_names(mky_names)
    
}

#' Run operation
#' 
#' Get monkey's operation, then execute in a vectorised fashion
#' 
#' @param monkey_name Numeric, the monkey's name
#' @param operation_override String, override of the operation to use
#' @param mky_info Parsed monkey info list
run_operation <- function(monkey_name, og_num, operation_override = NULL, mky_info = parsed_monkey_info) {
  
  old <- og_num
  
  # Access operation string
  operation_string <- if (is.null(operation_override)) {
    
    mky_info[[as.character(monkey_name)]]$operation
    
  } else operation_override
  
  # Run operation
  eval(parse(text = operation_string))
  
}

#' Perform division test
#' 
#' Get monkey's division test and execute it, return a boolean
#' 
#' @param monkey_name Numeric, the monkey's name
#' @param division_test_override Numeric, division test override
#' @param mky_info Parsed monkey info list
division_test <- function(monkey_name, current_worry, division_test_override = NULL, mky_info = parsed_monkey_info) {
  
  # Access division test number
  div_num <- if (is.null(division_test_override)) {
    
    mky_info[[as.character(monkey_name)]]$division_test
    
  } else as.numeric(division_test_override)
  
  # Perform division test
  test_result <- as.character(current_worry %% div_num == 0) %>% tolower()
  
  monkey_target <- mky_info[[as.character(monkey_name)]][[test_result]]
  
}

#' Sim(ian)ulate a number of full monkey rounds
#'
#' Run a full simulation of `rounds` number of rounds.
#'
#' @param mky_info List, output of `get_monkey_details()`
#' @param rounds Integer, number of rounds to simulate
simulate_monkey_business <- function(
  mky_info     = parsed_monkey_info, 
  rounds       = 3
) {
  
  # Starting values
  mky_info_wrk <- mky_info
  mky_names    <- names(mky_info)
  
  # Three key tables - 
  #  Initiate ordered monkey inventory
  inventory_list <- mky_info_wrk %>% 
    imap_dfr(~{
      
      tibble(
        holder  = .y,
        item_id = .x$items # We just need the number of items so we can assign IDs to them
      )
      
    }) %>% 
    mutate(item_id = 1:n(), .before = 1)
  
  #  Initiate worry tracker
  worry_df <- mky_info_wrk %>% 
    map_dfr(~tibble(worry_level = .x$items)) %>% 
    mutate(item_id = 1:n(), .before = 1)
  
  #  Initiate throws transaction table
  throws <- tibble(
    thrower  = character(), 
    receiver = character(),
    item_id  = numeric()
  )
  
  for (round in 1:rounds) {
    
    for (mky in mky_names) {
      
      # Monkey inspection 🙈 ----
      current_item <- inventory_list %>% 
        filter(holder == mky)
      
      if (nrow(current_item) == 0) next
      
      for (itm in 1:nrow(current_item)) {
        
        item_num   <- current_item[itm,]$item_id
        item_worry <- worry_df %>% filter(item_id == item_num) %>% pull(worry_level)
        
        # Run operation on worry. You are also relieved they didn't break the
        # item, your worry divides by 3 (floor(operated_worry / 3))
        new_worry_level <- floor(run_operation(mky, item_worry, mky_info = mky_info_wrk) / 3)
        
        #  Update worry level in worry_df
        worry_df <- worry_df %>% 
          rows_update(
            tibble(
              item_id     = item_num, 
              worry_level = new_worry_level
            ),
            by = 'item_id'
          )
        
        
        
        # Perform test to determine who to throw to next
        mky_target <- division_test(mky, current_worry = new_worry_level, mky_info = mky_info_wrk)
        
        if (item_num %in% 1:6 & round == 1) cat('Item ', item_num, ' worry: ', new_worry_level, '\n', '  Mky ', mky, ' to ', mky_target, ' division test ', mky_info_wrk[[mky]][['division_test']], ' modulo ', new_worry_level %% mky_info_wrk[[mky]][['division_test']], '\n')
        
        
        #  Execute throw -- Remove item row from the inventory list, then add
        #  back with new holder
        inventory_list <- inventory_list %>% 
          rows_delete(tibble(holder = mky, item_id = item_num), by = c('holder', 'item_id')) %>% 
          rows_insert(tibble(holder = mky_target, item_id = item_num), by = c('holder', 'item_id'))
        
        #  Log throw
        throws <- throws %>% 
          bind_rows(tibble(thrower = mky, receiver = mky_target, item_id = item_num))
        
      }
      
    }
    
  }
  
  return(
    list(
      inventory_list = inventory_list,
      worry_df       = worry_df,
      throws_df      = throws
    )
  )
  
}


### Analysis ----

parsed_monkey_info <- get_monkey_details(monkey_business)
sim_results        <- simulate_monkey_business(rounds = 20)

sim_results %>% 
  .$throws_df %>% 
  group_by(thrower) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  # Pull top 2 throws and multiply
  slice(1:2) %>% pull(n) %>% prod()

#' Answer is [78960]


## 11b Part 2 ----

# In this part of the question, our worry is no longer divided by 3 each time
# the item is inspected. This means our worry levels will quickly blow out of
# what we can store in the memory limits of integers and probably also bigints.

# Because of this, we'll need to redefine how we perform the test for which
# monkey to throw to since that is the only part that worry levels actually
# affect.

# The main components of the test are as follows:
# - The current worry level of the item, which is a product of multiple 
#   operations that led up to that point.
#    The issue is that the operations can be addition, multiplication or 
#    squaring, making storing the order of operations difficult.
#    Also, the documentation (?`%%`) states that "For double arguments, %% 
#    can be subject to catastrophic loss of accuracy if x is much larger than 
#    y, and a warning is given if this is detected"
# - The number to divide by (division_test)

# We're going to use modular arithmetic, which in a nutshell allows us to wrap
# numbers around the modulus. Another thing to note is that all division tests
# are prime, which might come in handy later.
#  - Instead of operating directly on the calculated worry level, we will take 
#    the modulus of the worry level after each operation is performed.
#  - We can then continue to operate on the modulus as usual, as long as we keep 
#    taking the modulus of the result.
#  - Note this works for addition, multiplication and squaring, but not division 
#    (we don't need to divide by anything here - if we did we would need to find 
#    the modular multiplicative inverse).

# This keeps our tracked number small (smaller than the modulus/division test),
# while still giving us the same answer - whether the number is divisible by the
# division test.

# For a good explanation on modular arithmetic, see this video:
# https://www.youtube.com/watch?v=lJ3CD9M3nEQ

#' Sim(ian)ulate a number of full monkey rounds using modular arithmetic
#'
#' Run a full simulation of `rounds` number of rounds using modular arithmetic
#' to perform the throw target tests.
#'
#' @param mky_info List, output of `get_monkey_details()`
#' @param rounds Integer, number of rounds to simulate
#' @param anxiety_mode Boolean, whether to skip reduction of worry by a factor
#'   of 3 each time a monkey inspects an item - as with the previous part of the
#'   question. Switching this to FALSE imitates the conditions of the first part
#'   of the question
simulate_monkey_business_modulararithmetic <- function(
  mky_info     = parsed_monkey_info, 
  rounds       = 10,
  anxiety_mode = TRUE
) {
  
  # Starting values
  mky_info_wrk <- mky_info
  mky_names    <- names(mky_info)
  
  # Function definitions ----
  
  #' Given a named list of values, calculate the modulus when compared against
  #' the modulus bases
  calculate_modnums <- function(list) {list %>% imap(~{.x %% moduli[[.y]]})}
  
  #' Given a named list of values and a monkey, run the monkey's operation
  #' across all values
  run_operations <- function(list, mky) {
    
    list %>% 
      map(~{run_operation(mky, .x)})
      
  }
  
  #' Given an item ID, run operation, then calculate moduli
  operate_and_mod <- function(item_id, mky, worry_list = worry_modnums, .anxiety_mode = anxiety_mode) {
    
    item_vals <- worry_list[[item_id]]
    
    worry_list[[item_id]] <- item_vals %>% 
      run_operations(mky = mky)  %>% 
      calculate_modnums() # %>% 
      # # This doesn't work when using modular arithmetic because we need to find
      # # the modular multiplicative inverse of 3 for each modulus base. Parked for now.
      # {if (.anxiety_mode) (.) else floor(. / 3)}%>% 
      # calculate_modnums()
    
    worry_list
    
  }
  
  #' Given a monkey, run the division test, which is just checking if the mod value is 0
  new_division_test <- function(item_id, mky, worry_list = worry_modnums, mky_info = mky_info_wrk) {

    # Perform division test
    test_result <- as.character(worry_list[[item_id]][[mky]] == 0) %>% tolower()
    
    mky_info[[as.character(mky)]][[test_result]]
    
  }

  
  # Three key data structures ----
  # Initiate ordered monkey inventory
  inventory_list <- mky_info_wrk %>% 
    imap_dfr(~{
      
      tibble(
        holder  = .y,
        item_id = .x$items # We just need the number of items so we can assign IDs to them
      )
      
    }) %>% 
    mutate(item_id = 1:n(), .before = 1)
  
  # Initiate worry tracker
  #  Get modulus base values
  moduli <- mky_info_wrk %>% map('division_test')
  
  #  Get list of items, then apply modulus base values
  worry_modnums <- mky_info_wrk %>% 
    map_dfr(~tibble(worry_level = .x$items)) %>% 
    pull(worry_level) %>% 
    map(~{

      item <- .x
      
      item %>% 
        map(~{

          moduli %>% 
            map(~{item %% .x})
          
        }) %>% .[[1]]
      
    })
  
  #  Initiate throws transaction table
  throws <- tibble(
    thrower  = character(), 
    receiver = character(),
    item_id  = numeric()
  )
  
  for (round in 1:rounds) {
    
    # print(round)
    
    for (mky in mky_names) {
      
      # Monkey inspection 🙈 ----
      current_item <- inventory_list %>% 
        filter(holder == mky)
      
      if (nrow(current_item) == 0) next
      
      for (itm in 1:nrow(current_item)) {
        
        item_num <- current_item[itm,]$item_id
        
        # Inspection is now composed of:
        #  - Run operation on worry (modulo version)
        #  - Take modulo of operated value
        #  - Run division test by checking if specific modulo for the given monkey equals 0
        
        # Run operation on modnums, then take modulo and update the modnums list
        worry_modnums <- operate_and_mod(item_num, mky)
        
        # Run division test
        mky_target <- new_division_test(item_num, mky)
        
        #  Execute throw -- Remove item row from the inventory list, then add
        #  back with new holder
        inventory_list <- inventory_list %>% 
          rows_delete(tibble(holder = mky, item_id = item_num), by = c('holder', 'item_id')) %>% 
          rows_insert(tibble(holder = mky_target, item_id = item_num), by = c('holder', 'item_id'))
        
        #  Log throw
        throws <- throws %>% 
          bind_rows(tibble(thrower = mky, receiver = mky_target, item_id = item_num))
        
      }
      
    }
    
  }
  
  return(
    list(
      inventory_list = inventory_list,
      worry_values   = worry_modnums,
      throws_df      = throws
    )
  )
  
}

# About 2 hours to run
# TODO: Optimise
sim_results_mod <- simulate_monkey_business_modulararithmetic(rounds = 10000) 

sim_results_mod %>% 
  .$throws_df %>% 
  group_by(thrower) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  # Pull top 2 throws and multiply
  slice(1:2) %>% pull(n) %>% prod()

#' Answer is [14561971968]
