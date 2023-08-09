# Define helper functions ----

#' Read input
#'
#' @param year A number indicating the AoC year of the input we're loading
#' @param q_num A number indicating which question to load the input for
#' @param read A boolean indicating whether to read the input in
#' @param read_fn Optionally a function used to read the input in
#' @param aoc_repo Optionally a string override for the working directory
#' @param verbose A boolean indicating whether to print logs
get_input <- function(
  year, 
  q_num, 
  read = TRUE,
  read_fn = readLines, 
  aoc_repo = NULL, 
  verbose = FALSE
) {
  
  # if (verbose) cat(paste('Current directory (should be the advent of code repo):', getwd()))
  
  target_directory <- if (!is.null(aoc_repo)) file.path(aoc_repo, year, 'inputs') else file.path(year, 'inputs')
  
  if (verbose) cat(paste('Checking for input in:', target_directory), '\n')
  
  file_dir <- max(list.files(target_directory, paste0('day_', q_num), full.names = TRUE))
  
  if (verbose) cat(paste('Reading', file_dir), '\n')
  
  if (read) {
    
    read_fn(file_dir)
    
  } else file_dir
  
}
