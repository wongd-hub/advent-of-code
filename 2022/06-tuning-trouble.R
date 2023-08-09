# 06 Tuning Trouble ----
#' https://adventofcode.com/2022/day/6

#' The elves have communication devices that receive each elf's location as a
#' series of letters, received one at a time. One such device is broken. To fix
#' it, we need to identify the start-of-packet marker which is a sequence of
#' four characters that are all different.
#' 
#' e.g. mjq[jpqm]gbljsphdztnvjfqwrcgsmlb
#'      The start-of-packet marker is complete on the 7th character because at
#'      that point, that and the previous 3 characters are different.
#'      
#' How many characters need to be processed before the first start-of-packet
#' marker is identified?

source(file.path('2022', 'utils', 'libs_and_funs.R'))

## 06a Part 1 ----

#' Raw text file - huge character string
datastream <- get_input(2022, 6)

#' Approach options:
#' - Split this string into staggered groups of 4 - 1:4, 2:5, 3:6, etc. and 
#'   then assess each one (possibly in parallel)
#' - "Crawl" across the string and assess each group of four characters one 
#'   at a time.
#'   
#' We'll go with the first option since this has the possibility of being 
#' parallelised later on if we want to.

#' Find start-of-packet marker
#'
#' Will decompose datastream into lagged subsets to check each for full
#' uniqueness. Returns the number of characters until the first start-of-packet
#' marker is encountered.
#'
#' @param datastream A string representing the datastream
#' @param num_characters An integer representing the number of characters that
#'   need to be unique for the start-of-packet marker
find_packet_marker <- function(data, num_characters = 4) {
  
  #' Create lagged versions of the datastream so that we can assess each one
  ds_list <- data %>% 
    #' Splitting character string into one character per vector element
    str_split_1('') %>% 
    #' Create matrix 
    embed(num_characters) %>% 
    #' Pull matrix rows into separate list items
    split(., seq(nrow(.))) %>% 
    #' `embed()` outputs the lagged vectors backwards, reverse them
    map(rev)
  
  #' Which sequence of x characters has x distinct characters?
  fully_unique <- ds_list %>% map_lgl(~{length(.x) == n_distinct(.x)})
  
  #' Number of characters before we get our first start-of-packet marker
  min(which(fully_unique)) + num_characters - 1

}

find_packet_marker(data = datastream)

#' Answer is [1655]

## 06b Part 2 ----

#' The start-of-packet marker is just part of the fix. We also need to find the
#' start-of-message marker to get messages. This is similar to the
#' start-of-packet marker but needs 14 distinct characters rather than 4. How
#' many characters need to be processed before the first start-of-message marker
#' is detected?

find_packet_marker(data = datastream, num_characters = 14)

#' Answer is [2665]