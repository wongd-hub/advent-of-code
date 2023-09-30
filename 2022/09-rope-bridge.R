# 09 Rope Bridge ----

#' You are modelling rope physics and you represent a rope in 2D space as a knot
#' for the head (H) and a knot for the tail (T). If the head is moved too far
#' away from the tail (in our case, the H and T must touch or overlap), the tail 
#' is pulled towards the head.
#'
#'  - If the H is ever 2 steps away from the T in the up, left, down, or right 
#'    directions, the T moves one step towards the H.
#'  - If the H and the T aren't touching, and aren't in the same row or column, 
#'    the T moves one step diagonally to keep up.
#'    
#' Examples:
#'  .....    .....    .....
#'  .....    ..H..    ..H..
#'  ..H.. -> ..... -> ..T..
#'  .T...    .T...    .....
#'  .....    .....    .....
#' 
#'  .....    .....    .....
#'  .TH.. -> .T.H. -> ..TH.
#'  .....    .....    .....
#'  
#' The puzzle input is a series of movements for the head in the form of: 
#' 
#'   [direction] [steps]
#' 
#' At the end of this, count up the number of positions that the tail visits
#' [at least once].
#' 
#' The puzzle context seems to suggest that we start both the H and T in the
#' same cell. Grid size is not defined, so lets use tuples/length-2 vectors to 
#' mark co-ords for now. Starting position doesn't matter, just how we move in 
#' relation to the starting position.

source(file.path('2022', 'utils', 'libs_and_funs.R'))


## 09a Part 1 ----
### Data ingestion ----

#' Raw text file
head_directions <- get_input(2022, 9)


### Function definitions ----

#' Instantiate origin point for head and tail
origin_coords <- list(
  h = c(x = 0, y = 0),
  t = c(x = 0, y = 0)
)


#' Calculate distance between two points
#' 
#' We need a way to calculate the number of steps away two different points are
#' on the grid. We're calculating from the perspective of the [subject] here;
#' i.e. which direction does the subject need to move to catch the target?
#' 
#' @param .subject_coord Vector of subject x and y co-ordinates
#' @param .target_coord Vector of target x and y co-ordinates
#' @param touch_assessment Whether to test if subject and target are touching
get_dist <- function(.subject_coord, .target_coord, touch_assessment = TRUE) {
  
  dist <- c(
    'x' = .target_coord[['x']] - .subject_coord[['x']], 
    'y' = .target_coord[['y']] - .subject_coord[['y']]
  )
  
  #' If no distances are smaller than or equal to 1 then H and T are [not]
  #' touching.
  if (touch_assessment) dist <- c(dist, 'touching' = !any(abs(dist) > 1))
  
  dist
  
}


#' Move subject towards target
#' 
#' @param subject_coord Vector of subject x and y co-ordinates
#' @param target_coord Vector of target x and y co-ordinates
#' @param move_factor How much to move T each time
#' @param verbose Boolean - whether to print logs
move_subject_to_target <- function(subject_coord, target_coord, move_factor = 1, verbose = F) {
  
  .sub_coord <- subject_coord
  .tar_coord <- target_coord
  
  dist <- get_dist(.subject_coord = .sub_coord, .target_coord = .tar_coord)
  
  if (verbose) {
    
    cat('NEW MOVE ----\n')
    cat('\tTarget:', .tar_coord, '\n\tSubject:', .sub_coord, '\n')
    cat('\tD:', dist[c('x', 'y')], ifelse(dist[['touching']] == 1, '- touching', '- NOT touching'), '\n')
    
  }
  
  if (dist[['touching']] != 1) {
    
    if (verbose) cat('\tPoints not touching, moving subject towards target\n')
    
    #' If in up, down, left, right direction - straightforward, make a move
    if (any(dist[1:2] == 0)) {
      
      if (verbose) cat('\tNon-diagonal direction, moving on the x or y axis only\n')
      
      #' Should only be one direction that is non-zero, as if both are zero then
      #' dist$touching should be TRUE. Find the direction that is non-zero.
      move_direction <- names(dist)[which(dist[1:2] != 0)]
      
      #' Move the co-ord of the tail in the direction of the head, use `sign` to
      #' normalise the vector direction first
      .sub_coord[[move_direction]] <- .sub_coord[[move_direction]] + move_factor * sign(dist[[move_direction]])
      
      if (verbose) cat('\tMove:', move_direction, move_factor * sign(dist[[move_direction]]), '\n')
      
    } else {
      
      if (verbose) cat('\tDiagonal direction, moving on both x and y axes\n')
      
      #' If diagonal (equivalent to `!any(dist[1:2] == 0)`), move diagonally towards head
      
      #' Figure out x and y direction to move, then move in that direction diagonally
      .sub_coord[['x']] <- .sub_coord[['x']] + move_factor * sign(dist[['x']])
      .sub_coord[['y']] <- .sub_coord[['y']] + move_factor * sign(dist[['y']])

      if (verbose) cat('\tMove:', move_factor * sign(dist[['x']]), move_factor * sign(dist[['x']]), '\n')
      
    }

  }
  
  list(
   subject = .sub_coord,
   target  = .tar_coord
  )
  
}


#' Move H and make T follow
#' 
#' @param initial_coords A list of initial H and T coordinates
#' @param instruction_list Character vector of instructions
#' @param movement_size_h Movement step size - how much to move H each step
#' @param movement_size_t Movement step size - how much to move T each step
#' @param verbose Boolean determining whether to print logs
follow_h <- function(
  initial_coords   = NULL,
  instruction_list = NULL,
  movement_size_h  = 1,
  movement_size_t  = 1,
  verbose          = F
) {
  
  if (any(is.null(initial_coords), is.null(instruction_list))) 
    stop('Please provide both initial coordinates and an instruction list')
  
  if (verbose) cat('Begin parsing instructions\n')
  
  #' Init objects/results
  coords_wrk  <- initial_coords
  results_lst <- vector('list', sum(str_sub(instruction_list, 3) %>% as.numeric()) * movement_size_h + 1)
  
  results_lst[[1]] <- tibble(
    t_x = initial_coords$t[['x']],
    t_y = initial_coords$t[['y']],
    h_x = initial_coords$h[['x']],
    h_y = initial_coords$h[['y']],
  )
  
  #' Hard to figure out which item in the list we need to assign to using just
  #' `i` and `j` since each instruction has a different number of instructions
  #' in it. Easier to set up a separate counter and increment it each time we
  #' add a new coordinate to the table.
  overall_counter <- 2
  
  #' Loop through instructions
  for (i in seq_along(instruction_list)) {
    
    if (verbose) cat('  Parsing instruction:', instruction_list[[i]], '\n')
    
    direction <- str_sub(instruction_list[[i]], 1, 1)
    steps     <- str_sub(instruction_list[[i]], 3) %>% as.numeric()
    
    direction_signs_h <- list(
      U = c(x = 0, y = movement_size_h),
      D = c(x = 0, y = -movement_size_h),
      L = c(x = -movement_size_h, y = 0),
      R = c(x = movement_size_h, y = 0)
    )
    
    if (verbose) 
      cat('   ', glue('Start - H: ({coords_wrk$h[["x"]]}, {coords_wrk$h[["y"]]}) T: ({coords_wrk$t[["x"]]}, {coords_wrk$t[["y"]]})'), '\n')
    
    for (j in seq_len(steps)) {
      
      if (verbose) cat('   ', glue('Moving H {direction} by {movement_size_h}...'))
      
      # Move H a step in the direction instructed
      coords_wrk$h <- c(
        x = coords_wrk$h[['x']] + direction_signs_h[[direction]][['x']],
        y = coords_wrk$h[['y']] + direction_signs_h[[direction]][['y']]
      )
      
      if (verbose) cat(' ', glue('H: ({coords_wrk$h[["x"]]}, {coords_wrk$h[["y"]]})'), '\n')
      
      coords_wrk_tmp <- move_subject_to_target(coords_wrk$t, coords_wrk$h, move_factor = movement_size_t) %>% 
        set_names(c('t', 'h'))
      
      if (!identical(coords_wrk, coords_wrk_tmp) & verbose) {
        cat('   ', glue('T not touching H, moved from ({
          coords_wrk$t[["x"]]}, {coords_wrk$t[["y"]]
        }) to ({
          coords_wrk_tmp$t[["x"]]}, {coords_wrk_tmp$t[["y"]]
        })'), '\n')
      } else if (verbose) {
        cat('   ', glue('T touching H still - no movement, stays at ({coords_wrk_tmp$t[["x"]]}, {coords_wrk_tmp$t[["y"]]})'), '\n')
      }
      
      results_lst[[overall_counter]] <- tibble(
        t_x = coords_wrk_tmp$t[['x']],
        t_y = coords_wrk_tmp$t[['y']],
        h_x = coords_wrk_tmp$h[['x']],
        h_y = coords_wrk_tmp$h[['y']],
      )
      
      overall_counter <- overall_counter + 1
      
      coords_wrk <- coords_wrk_tmp
    
    }
    
  }
  
  list(
    final_coord = coords_wrk,
    coord_tracker = bind_rows(results_lst)
  )
  
}

### Answer ----

normal_rope_results <- follow_h(
  origin_coords, 
  head_directions, 
  verbose = F
)$coord_tracker

normal_rope_results %>% 
  distinct(t_x, t_y) %>% 
  nrow()

#' Answer is [6745]

## 09b Part 2 ----

#' We now have to simulate a rope with 10 knots instead of 2: H, 1, 2, ..., 9.
#' This is an extension of the original problem, where the 1 tracks where the T
#' used to go, and the 2-9 are traces of where 1 has been in the last 1-8 steps
#'
#' We use the same instructions as the first part, and want to track the number
#' of places where the 9 has been.
#'
#' Note that different kinds of movements are possible due to the new length of
#' the rope. For example, the 2-9 won't exactly track where the 1 has been if
#' the one moves to a spot where it would not touch the 2 anymore. We need a way
#' to apply the `move_subject_to_target` function to [each knot in the rope].

#' Move H and make knots in rope follow
#' 
#' @param initial_coords A list of initial H and knot coordinates; the order of
#'   knot coordinates provided matters as each knot is taken sequentially in the
#'   rope, number of knots is taken from the length of this list. First knot must 
#'   be named 'h'.
#' @param instruction_list Character vector of instructions
follow_h_longer_rope <- function(
  initial_coords   = NULL,
  instruction_list = NULL
) {

  #' Init objects/results collector
  coords_wrk  <- initial_coords
  results_lst <- vector('list', length(coords_wrk)) %>% 
    map(~{
      vector('list', sum(str_sub(instruction_list, 3) %>% as.numeric()) + 1)
    }) %>% 
    set_names(names(coords_wrk))
  
  #' Set up a separate counter and increment it each time we add a new
  #' coordinate to the table.
  overall_counter <- 2
  
  direction_signs_target <- list(
    U = c(x = 0, y = 1),
    D = c(x = 0, y = -1),
    L = c(x = -1, y = 0),
    R = c(x = 1, y = 0)
  )
  
  #' Loop through instructions
  for (i in seq_along(instruction_list)) {
    
    direction <- str_sub(instruction_list[[i]], 1, 1)
    steps     <- str_sub(instruction_list[[i]], 3) %>% as.numeric()
    
    for (j in seq_len(steps)) {
      
      for (knot_i in seq_along(coords_wrk)) {
        
        if (names(coords_wrk)[knot_i] == 'h' & knot_i == 1) {
          
          #' Handling 'H'
          #' Move H a step in the direction instructed
          coords_wrk$h <- c(
            x = coords_wrk$h[['x']] + direction_signs_target[[direction]][['x']],
            y = coords_wrk$h[['y']] + direction_signs_target[[direction]][['y']]
          )
          
        } else {
          
          #' Otherwise, handle the second knot on-wards
          coords_wrk_tmp <- move_subject_to_target(coords_wrk[[knot_i]], coords_wrk[[knot_i - 1]])
          
          if (!identical(coords_wrk[[knot_i]], coords_wrk_tmp$subject)) {
            
            coords_wrk[[knot_i]] <- coords_wrk_tmp$subject
            
          }
          
        }
        
        #' Save down to results list
        if (overall_counter == 2) {
          
          #' If on first iteration, also save down origin points
          results_lst[[knot_i]][[1]] <- tibble(
            x = initial_coords[[knot_i]][['x']],
            y = initial_coords[[knot_i]][['y']]
          )
          
        }
        
        results_lst[[knot_i]][[overall_counter]] <- tibble(
          x = coords_wrk[[knot_i]][['x']],
          y = coords_wrk[[knot_i]][['y']]
        )
        
      }

      overall_counter <- overall_counter + 1
      
    }
    
  }
  
  #' Return
  list(
    final_coord   = coords_wrk,
    coord_tracker = results_lst %>% 
      lapply(bind_rows) %>% 
      bind_rows(.id = 'knot')
  )
  
}

#' Setting up initial co-ordinates list
list_items <- c('h', 1:9) # 'Automatically coerces to character
initial_coord_points <- list_items %>% 
  lapply(\(x) c('x' = 0, 'y' = 0)) %>% 
  set_names(list_items)

longer_rope_results <- follow_h_longer_rope(
  initial_coord_points, 
  head_directions, 
  verbose = F
)$coord_tracker 

longer_rope_results %>% 
  filter(knot == '9') %>%
  distinct(x, y) %>% 
  nrow()

#' Answer is [2793]