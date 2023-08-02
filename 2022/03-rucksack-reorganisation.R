# 03 Rucksack Reorganization ----
#' https://adventofcode.com/2022/day/3

#' Elves need to pack rucksacks but it appears that the rules weren't followed
#' when doing so.
#'
#' - Each rucksack has two compartments. All items of a given type (denoted by a 
#'   single character [a-zA-Z]) are designed to go into only one of the two rucksack 
#'   compartments.
#' - Each rucksack's contents are given by all characters on a single line. The first 
#'   half of the letters refers to the first compartment of the rucksack, and the same 
#'   for the second half.
#'    - So based on the previous point, you can't have the same letter in both halves 
#'      of a string (you can have it be duplicated in the one half though)
#'      
#'      e.g. ttgJtRGJQctTZtZT
#'           't' shows up in both halves which is an issue. The fact that t is 
#'           duplicated within each half isn't an issue.
#'           
#' To help prioritise item rearrangement, each item type can be converted to a priority.
#' 
#'  - a-zA-Z => 1-52; so in the example above, the priority of the letter that appears 
#'    in both rucksack compartments (t) is 20
#'    
#' Find the item type that appears in both compartments of each rucksack. What is the 
#' sum of the priorities of those item types?

library(tidyverse)
library(microbenchmark)

## 03a Part 1 ----
### Loading data ----

#' Raw text file => one rucksack per line character vector
day_3_input_fp <- max(list.files(file.path('2022', 'inputs'), 'day_3', full.names = TRUE))
rucksack_contents <- readLines(day_3_input_fp)

#' Priorities // `letters` is a built in vector that contains all alphabet letters
priority_tbl <- c(letters, toupper(letters)) %>% 
  tibble(
    letter = .,
    priority = seq_along(.)
  )


### Testing functions ----
#' Build function possibilities to find item types that appear in both
#' compartments of a rucksack
#'  Expect this to be faster since it mainly deals with vectors
find_duplicated_items_attempt_1 <- function(rucksack_string) {
  
  #' Deconstruct rucksack
  deconstructed_rucksack <- str_split_1(rucksack_string, '')
  
  intersect(
    deconstructed_rucksack[1:(nchar(rucksack_string)/2)],
    deconstructed_rucksack[((nchar(rucksack_string)/2) + 1):nchar(rucksack_string)]
  )
  
}

#'  Expect this to be slower since it uses tibbles and joins
find_duplicated_items_attempt_2 <- function(rucksack_string) {
  
  #' Deconstruct rucksack
  deconstructed_rucksack <- str_split_1(rucksack_string, '')
  
  list(
    deconstructed_rucksack[1:(nchar(rucksack_string)/2)],
    deconstructed_rucksack[((nchar(rucksack_string)/2) + 1):nchar(rucksack_string)]
  ) %>% 
    map(~{
      tibble(char = .x) %>% 
        group_by(char) %>% 
        tally() %>% 
        ungroup()
    }) %>% 
    reduce(
      full_join,
      by = 'char',
      suffix = c('_1', '_2')
    ) %>% 
    filter(!is.na(n_1) & !is.na(n_2)) %>% 
    .$char
  
}

if (F) {
  
  microbenchmark(
    map(rucksack_contents[1:100], find_duplicated_items_attempt_1),
    map(rucksack_contents[1:100], find_duplicated_items_attempt_2)
  )
  
  #> Unit: milliseconds
  #>                                                           expr        min         lq       mean     median         uq       max neval
  #> map(rucksack_contents[1:100], find_duplicated_items_attempt_1)   4.205167   4.326751   4.672882   4.379354   4.435001  16.52721   100
  #> map(rucksack_contents[1:100], find_duplicated_items_attempt_2) 674.846292 688.558501 702.241635 699.816376 704.793001 902.64738   100
  
  #' find_duplicated_items_attempt_1 is 150x faster, but gives us less information along the way
  
}


### Running analysis ----

rucksack_contents %>% 
  map_chr(find_duplicated_items_attempt_1) %>%
  # # How many duplicated letters per rucksack?
  # sapply(length) %>% unique() # Only 1 for each
  tibble(to_rearrange = .) %>% 
  left_join(priority_tbl, by = c('to_rearrange' = 'letter')) %>% 
  .$priority %>% sum()

#' Answer is [8105]

## 03b Part 2 ----

#' All elves are organised into groups of three, and each elf within each group
#' carries a badge that identifies that group. This badge is the ONLY item that
#' is carried by all three elves in a group.
#' 
#' - Need to find the item that is common across all three elves in each group
#' - Every set of three lines corresponds to one group
#' - Sum the priorities across all badge items (should be 100 badges since there
#'   are 300 rucksacks)

#' Split rucksack_contents into length-three character vectors first
split(
  rucksack_contents,
  ceiling(seq_along(rucksack_contents) / 3)
) %>% 
  map_chr(~{
    .x %>% 
      map(~str_split_1(.x, '')) %>% 
      reduce(intersect)
  }) %>% 
  tibble(badge_items = .) %>% 
  left_join(priority_tbl, by = c('badge_items' = 'letter')) %>% 
  .$priority %>% sum()

#' Answer is [2363]