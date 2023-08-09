# 01 Calorie Counting ----
#' https://adventofcode.com/2022/day/1

#' Text file provided, every line is a calorie count
#'  - Every newline denotes a new elf, ie. 
#'  
#'    1000
#'    2000
#'    3000
#'  
#'    2000
#'  
#'    Means that elf 1 is carrying 6000 calories, and elf 2 is carrying 2000

source(file.path('2022', 'utils', 'libs_and_funs.R'))

## 01a Part 1 ----

#' Elves need to know which elf to ask if they get hungry - ie. which elf has the most calories on them?
#' readLines puts every new line into an element of a character vector
calories_raw <- get_input(2022, 1) |> 
  # Convert characters to numerics. Blank/newlines become NA
  as.numeric()

#' We can use these NAs to split this vector by using `split()`
#'  First, we create the grouping factor this function uses
#'  ie. Split the vector into groups based on the preceding NA
factor_grouping <- cumsum(is.na(calories_raw))

#'  Then split the calories_raw vector (removing NAs)
calories_split <- split(calories_raw[!is.na(calories_raw)], f = factor_grouping[!is.na(calories_raw)])

#' We've now got a list of vectors which is easier to work with; sum each vector
#' and find the max of the sums
calories_split |>
  sapply(sum) |>
  max()

#' Answer is [68802]

## 01b Part 2 ----

#' The elves now want to know the total calories carried by the top 3 elves.

calories_sorted <- calories_split |>
  # Sum each vector again
  sapply(sum) |>
  # Sort in descending order
  sort(decreasing = TRUE)

# Get sum of top 3 elves
calories_sorted[1:3] |> sum()

#' Answer is [205370]