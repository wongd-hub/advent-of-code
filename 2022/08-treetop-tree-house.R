# 08 Treetop Tree House ----
#' https://adventofcode.com/2022/day/8

#' You come across a grid of trees. We want to see how many of the trees are
#' visible from the outside of the grid for the elves to build a treehouse. A
#' tree is only visible from outside the grid if:
#' 
#'  - The tree is on the outskirts of the grid (ie. is in the outside layer of 
#'    the grid)
#'  - The tree is taller than all other trees from any of the left, top, right, 
#'    or bottom sides
#'  
#' e.g.
#'  30373
#'  25512
#'  65332
#'  33549
#'  35390
#' 
#'  - The top left 5 is visible from the top and left
#'  - The top right 1 is not visible

source(file.path('2022', 'utils', 'libs_and_funs.R'))

## 08a Part 1 ----
### Data ingestion ----

#' Raw text file -> character vector where each string is a row in the tree grid
tree_grid <- get_input(2022, 8)

#' We want to turn this into a matrix for easier extraction
tree_matrix <- tree_grid %>% 
  map(~{.x %>% str_split_1('') %>% as.numeric()}) %>% 
  do.call(rbind, .)

### Problem setup ----

#' We want to take every i/j pair (row i, column j) and determine whether all
#' the heights in all four directions are smaller

#' Generate i/j pairs
ij_pairs <- crossing(i = 1:nrow(tree_matrix), j = 1:ncol(tree_matrix))

#' We need a function that will extract vectors in all directions from a certain
#' i/j co-ordinate. If a vector is length 1, that means the tree is on the outer
#' layer, so by default its visible.

#' Determine if a tree is visible from the outside
#'
#' @params i, j i/j co-ordinates for the target tree
tree_visibility <- function(i, j) {
  
  target_tree_height <- tree_matrix[[i, j]]
  
  sightlines <- list(
    #' Grab everything from the border to just before the target tree, if it is
    #' on the border then output `-1` which will always output a TRUE to the
    #' second part of this pipe
    left   = if (j == 1) c(-1) else tree_matrix[i, 1:(j - 1)],
    top    = if (i == 1) c(-1) else tree_matrix[1:(i - 1), j],
    right  = if (j == ncol(tree_matrix)) c(-1) else tree_matrix[i, (j + 1):ncol(tree_matrix)],
    bottom = if (i == nrow(tree_matrix)) c(-1) else tree_matrix[(i + 1):nrow(tree_matrix), j]
  ) %>% 
    #' If any of the elements in the vector are equal to or above the height of
    #' the target tree, it's not visible
    map_dfc(~{
      all(.x < target_tree_height)
    })
    
  sightlines %>%
    #' Are any of the directions TRUE?
    mutate(visible_any = any(left, top, right, bottom)) %>% 
    #' Add co-ordinates to results
    mutate(i = i, j = j, .before = 1)
  
}

### Answer ---
tree_vis_results <- ij_pairs %>% pmap_dfr(tree_visibility)

sum(tree_vis_results$visible_any)

#' Answer is [1798]

## 08b Part 2 ----

#' We now want to calculate a 'scenic score' that is a function of how many
#' trees you can see from a given tree in all 4 directions. Look in all 4
#' directions until you see a tree taller than or equal in height to the current
#' tree. Multiply the amount of trees you can see in each direction to get the
#' scenic score. If a tree is on the edge, its viewing score in that direction
#' is 0.
#' 
#' What is the highest scenic score possible for any tree?

#' Determine if a tree is visible outside the grid
#'
#' @params i, j i/j co-ordinates for the target tree
scenic_score <- function(i, j) {
  
  target_tree_height <- tree_matrix[[i, j]]
  
  view_distances <- list(
    #' Grab everything from the border to just before the target tree, if it is
    #' on the border then output `0`
    #' 
    #' We're using `rev` here to order all vectors to read from the 'inside'-out
    left   = if (j == 1) c(0) else rev(tree_matrix[i, 1:(j - 1)]),
    top    = if (i == 1) c(0) else rev(tree_matrix[1:(i - 1), j]),
    right  = if (j == ncol(tree_matrix)) c(0) else tree_matrix[i, (j + 1):ncol(tree_matrix)],
    bottom = if (i == nrow(tree_matrix)) c(0) else tree_matrix[(i + 1):nrow(tree_matrix), j]
  ) %>% 
    #' If any of the elements in the vector are equal to or above the height of
    #' the target tree, it's not visible
    map_dfc(~{
      if (sum(.x) == 0 & length(.x) == 1) {
        #' If the vector was given a 0 score from above, set score to 0
        0
      } else {
        which(.x >= target_tree_height) %>% 
          min(length(.x))
      }
    })
  
  view_distances %>%
    #' Are any of the directions TRUE?
    mutate(scenic_score = left * top * right * bottom) %>% 
    #' Add co-ordinates to results
    mutate(i = i, j = j, .before = 1)
  
}

### Answer ---
scenic_score_results <- ij_pairs %>% pmap_dfr(scenic_score)

max(scenic_score_results$scenic_score)

#' Answer is [1798]