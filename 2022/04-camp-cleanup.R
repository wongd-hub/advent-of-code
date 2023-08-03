# 04 Camp Cleanup ----
#' https://adventofcode.com/2022/day/4

#' The elves need to clean up sections of the camp, however they've noticed that
#' the sections they've been assigned sometimes overlap with their partner's
#' sections. The full roster is provided in the following format: elf_1,elf_2
#' 
#' e.g. '2-4,6-8' means elf 1 cleans sections 2, 3, 4; elf 2 cleans 6, 7, 8, etc.
#' 
#' Some elf pairs have instructions that completely engulf their partner's sections,
#' e.g. '2-4,1-8'.
#' 
#' In how many assignment pairs does one range fully contain the other?

library(tidyverse)
library(microbenchmark)

## 04a Part 1 ----
### Answer ----

#' Raw text file => pair-wise camp section assignments
day_4_input_fp <- max(list.files(file.path('2022', 'inputs'), 'day_4', full.names = TRUE))
camp_sections  <- readLines(day_4_input_fp)

#' Split the strings down into elf 1 and elf 2, then down again into segment
#' from/to pairs for each elf. Convert final numbers to numerics otherwise 
#' inequalities will not work correctly.
split_rosters <- camp_sections %>% 
  #' Returns a matrix, closest we get to a vector with this family of functions
  #' for the use case here(?)
  str_split_fixed(',', 2) %>% 
  #' Across each matrix columns, split down into from/to, then convert to
  #' numeric
  apply(2, \(x) str_split_fixed(x, '-', 2) %>% apply(2, as.numeric), simplify = FALSE)

#' Rename elements that its a bit more clear whats happening in the following block
elf_1 <- list(
  from = split_rosters[[1]][,1],
  to   = split_rosters[[1]][,2]
)
elf_2 <- list(
  from = split_rosters[[2]][,1],
  to   = split_rosters[[2]][,2]
)

#' Since each elf's segment range is contiguous - to find engulfed pairs, check 
#' if one's min and max are smaller and greater than (respectively) the other's min 
#' and max. Then turn it around and do it vice versa.
engulfed_pairs <- ((elf_1$from <= elf_2$from) & (elf_1$to >= elf_2$to)) |
  ((elf_2$from <= elf_1$from) & (elf_2$to >= elf_1$to))

#' Sum all TRUEs (ie. engulfed pairs)
sum(engulfed_pairs)

#' Answer is [485]

### Archive ----

if (F) {
  
  microbenchmark(
    old_ver = {
      #' This was the first attempt; using element-wise map isn't as fast/efficient
      #' as thinking mainly with vectors.
      
      #' Check for complete containment from one elf to their partner
      #' 
      #' Returns the elf who engulfs the other, else NA
      #' 
      #' @params elf_1, elf_2 Each a list containing a `from` and a `to` representing
      #'   the start and end of each elf's range
      check_complete_containment <- function(elf_1, elf_2) {
        
        if ((elf_1$from <= elf_2$from) & (elf_1$to >= elf_2$to)) {
          'elf_1'
        } else if ((elf_2$from <= elf_1$from) & (elf_2$to >= elf_1$to)) {
          'elf_2'
        } else NA_character_
        
      }
      
      complete_engulf <- camp_sections %>% 
        #' For each roster pair...
        map(~{
          
          #' ... split the string on the comma so you have a list of two ranges
          #' represented like '24-58'
          full_range <- str_split_1(.x, ',') %>% 
            map(~{
              
              #' For each range (e.g. '24-58'), split into list of from and to (i.e.
              #' list(from = 24, to = 58))
              section_range <- str_split_1(.x, '-') %>% 
                as.numeric() %>% 
                as.list() %>% 
                set_names('from', 'to')
              
              #' A leftover from the first attempt...
              # do.call(partial(seq, by = 1), section_range)
              
              section_range
              
            })
          
          #' Could get the range and turn it into a vector of numbers; however
          #' that doesn't seem performant and performance can vary based on how
          #' big each range is.
          
          #' A better way might be to check if one's min and max are smaller than
          #' and greater than (respectively) the other's min and max. Then turn it 
          #' around and do it vice versa. This can be done since all ranges are 
          #' contiguous.
          
          check_complete_containment(full_range[[1]], full_range[[2]])
          
        })
      
      #' Get the number of non-NA (i.e. completely engulfed) pairs
      sum(!is.na(complete_engulf))
      
      #' Answer is [485]
    },
    new_ver = {
      #' Using the method used in the final answer
      
      split_rosters <- camp_sections %>% 
        str_split_fixed(',', 2) %>% 
        apply(2, \(x) str_split_fixed(x, '-', 2) %>% apply(2, as.numeric), simplify = FALSE)
      
      elf_1 <- list(
        from = split_rosters[[1]][,1],
        to   = split_rosters[[1]][,2]
      )
      elf_2 <- list(
        from = split_rosters[[2]][,1],
        to   = split_rosters[[2]][,2]
      )
      
      engulfed_pairs <- ((elf_1$from <= elf_2$from) & (elf_1$to >= elf_2$to)) |
        ((elf_2$from <= elf_1$from) & (elf_2$to >= elf_1$to))
      
      sum(engulfed_pairs)
      
      #' Answer is [485]
    },
    times = 50L
  )
  
  #> Unit: milliseconds
  #>    expr        min         lq       mean    median         uq        max neval
  #> old_ver 124.307126 127.913626 134.436668 135.38242 140.338626 146.644542    50
  #> new_ver   3.728459   3.822543   3.967556   3.92698   4.028084   4.562042    50

  #' Newer version is 34x faster than the old
  
}


## 04b Part 2 ----

#' Now the elves want to know who has any overlapping work at all. Since ranges
#' are contiguous, we can answer this by changing up the inequality.
#' 
#' Might be easier to go for the complement here; ie. how many pairs DO NOT 
#' overlap at all? that would mean either:
#' - Elf 1 To < Elf 2 From; or,
#' - Elf 2 To < Elf 1 From

overlapping_pairs <- !(
  (elf_1$to < elf_2$from) |
    (elf_2$to < elf_1$from)
)

#' We expect this to be larger than the previous answer since this is a superset
#' of that
sum(overlapping_pairs)

#' Answer is [857]