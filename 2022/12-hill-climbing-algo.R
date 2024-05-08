# 12 Hill Climbing Algorithm ----

#' You want to climb to where you can get the best signal. Your handheld device
#' gives you a heightmap of the area which is the puzzle input. Each square of
#' the area is represented by a lowercase letter where 'a' is the lowest
#' elevation and 'z' is the highest.
#'
#' Your location is marked ('S') and has elevation 'a'. Your goal is to get to
#' 'E' which has elevation 'z'. You want to get there in as few steps as
#' possible and you can only climb up one elevation level per step (but you can
#' drop as many as you need).

source(file.path('2022', 'utils', 'libs_and_funs.R'))

## 12a Part 1 ----
### Data ingestion ----

#' Raw text file
heightmap <- get_input(2022, 12)

heightmap <- c(
  'Sabqponm',
  'abcryxxl',
  'accszExk',
  'acctuvwj',
  'abdefghi'
)

get_loc_of_first_occurrence <- function(search_string, data = heightmap) {
  
  x_coord <- str_which(data, search_string)[[1]]
  
  y_coord <- str_split(data[[x_coord]], '')[[1]] %>% 
    str_which(search_string) %>% 
    .[[1]]
  
  return (c('x' = x_coord, 'y' = y_coord))
  
}

### Analysis ----

letter_mapping <- 1:length(base::letters) %>% set_names(base::letters)

locs <- list(
  start = get_loc_of_first_occurrence('S'),
  end   = get_loc_of_first_occurrence('E'),
)


numeric_heightmap <- heightmap %>% 
  str_replace('S', 'a') %>% 
  str_replace('E', 'z') %>% 
  map(~{
    
    str_split(.x, '')[[1]] %>% 
      map_chr(~letter_mapping[[.x]]) %>% 
      matrix(ncol = length(.))
    
  }) # How do we get this into matrix form? stack() doesn't work


### Function definitions ----

# We need:
#  - A function that can assess all possible movements (U, D, L, R) for feasibility (either current elevation + 1 or <= current_elevation)
#  - An orchestration function that:
#     - Sees the starting point and assesses points around that
# Number of routes will grow very fast as number of potential squares increases (processing time, memory constraints). How do we handle this?
# Is there some sort of shortest path algorithm or heuristic?
# Is there a way of discarding routes early that are proving to be too long?



