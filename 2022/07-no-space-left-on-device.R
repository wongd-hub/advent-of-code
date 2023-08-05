# 07 No Space Left On Device ----
#' https://adventofcode.com/2022/day/7

#' You are trying to update the device but have run out of space. You decide to
#' browse the file system to see what you can delete.
#'
#' - The file system is composed of files and directories, the outermost directory is /
#' - Console commands are the same as bash - `cd` and `ls` are the main functions used
#'   - `dir xyz` means there exists a directory called `xyz` in the current folder
#'   - `[num] xyz` means there exist a file in the current directory with size [num]
#'   - Commands input by the user start with a `$`
#'
#' The goal is to get the size of each directory (at every level). For the first
#' part of the question, get the size of all directories under 100k in size and
#' sum them (allowing for double-counting files).
#' 
#' Assume directories don't have any inherent size themselves.


## 07a Part 1 ----
### Data ingestion ----
#' Raw text file - use empty line to delimit the crate drawing and the instructions.
day_7_input_fp <- max(list.files(file.path('2022', 'inputs'), 'day_7', full.names = TRUE))
system_commands <- readLines(day_7_input_fp)

#' Two things we need to do 
#' - Determine the directory structure
#' - Determine the files in each directory

#' First, how do we represent this with R data constructs? 
#' - Could we represent the directory structure as nested lists, e.g.:
#'     list(
#'       dir_a = list('file.txt', 'file1.txt', 'dir_b'),
#'       dir_b = list('file3.txt)
#'     )
#' - Then have a look-up table with all file sizes, easy to create; just run regex 
#'   over entire list of commands
#' - It appears that not all files have filetypes ('.yyy'), so prefix with 'file:'

### Revealing the file system ----

#' Pseudocode
#  For every cd, record the overall filepath (/ + bcfwbq + ...), handling '..' as moving back one folder
#  For every ls, populate the current working folder with the contents

command_crawler <- function(
  commands, 
  symbols = list(
    dir_sep = "/",
    user_input = "$", 
    change_dir = "cd", 
    list_dir_contents = "ls",
    dir_prefix = "dir",
    file_prefix = "[0-9]+",
    updir = '..'
  ),
  verbose = FALSE
) {
  
  #' Can't just extract all `$ cd`s, in case there is a directory found via 
  #' `$ ls` that is not then `cd`d into by the user
  
  filesystem <- list()
  
  #' Change the part of the filesystem list that we're in to emulate the working directory
  #' PULL THIS INTO A SEPARATE FUNCTION
  
  #' We know the filesystem starts a '/'. When we see a `cd`, we need to know
  #' where it is in relation to '/'.
  
  #' The top level of this list represents the home directory. When we see a
  #' `cd`, check to see where we are in relation to home (`cd`s without a '/'
  #' in front of the path are [relative], whereas those that start with '/'
  #' are [absolute] from the perspective of the home directory)
  dir_tree <- list()
  current_directory <- '~'
  
  
  #' We also need to be able to handle '..'s which mean to move up one
  #' directory level. We won't handle this directly in the `case_when()`
  #' because of the complexity if this were passed in as part of a larger
  #' command (e.g. "hello/../world"). Instead, we'll check the current
  #' directory for '..'s every time it's updated and remove them + the
  #' elements immediately preceding them to simulate moving up one dir lvl.
  
  #' Handle '..'s in filepath
  #'
  #' @param directory A character vector representing the current directory,
  #'   each element being a directory level
  handle_updir <- function(string, updir_sym = symbols$updir) {
    
    #' Get location of '..'s and also the directories one level above
    updir_markers <- which(string == updir_sym)
    prev_dir <- updir_markers - 1
    
    to_remove <- c(prev_dir, updir_markers)
    if (any(to_remove < 1)) stop('Internal error: an ".." is attempting to go above the home directory level')
    
    string[-to_remove]
    
  }
  
  
  #' We need a way to traverse a nested list and add new empty lists at any
  #' level as well.
  
  #' Recursive add
  #'
  #' @param list A list to recursively modify
  #' @param path A character vector containing the directory levels to check
  recursive_add <- function(list, path) {
    
    # If the path is empty, return the list - our success condition
    if(length(path) == 0) return(list)
    
    # Split the path into the current directory and the rest
    target_dir <- path[1]
    rest <- path[-1]
    
    # If the current part doesn't exist in the list, add it
    if(!target_dir %in% names(list)) list[[target_dir]] <- list()
    
    # Walk down the list, and recursively modify the next level
    list[[target_dir]] <- recursive_add(list[[target_dir]], rest)
    
    return(list)
    
  }
  
  
  for (i in 1:length(commands)) {
    
    if (verbose) cat('Parsing command', i, '\n')
    
    #' If the command is to change directory...
    if (str_detect(commands[[i]], fixed(paste(symbols$user_input, symbols$change_dir)))) {
      
      # PULL INTO SEPARATE FUNCTION? Too many args probably
      
      #' Get directory we're changing to
      dir_target <- str_remove(commands[[i]], fixed('$ cd '))
      
      #' Find where we are in the directory tree
      #'   Can't use `case_when()` since it recycles RHS to the largest RHS
      #'   output supplied. Use if-else instead.
      current_directory <- if (
        #' If we're navigating to the home directory - move us back there
        dir_target == symbols$dir_sep
      ) {
        '~'
      } else if (
        #' Else if the path is an absolute path (starts with a '/') then build
        #' the full path, splitting by '/'
        str_sub(dir_target, 1, 1) == symbols$dir_sep & nchar(dir_target) > 1
      ) {
        str_split_1(dir_target, '/') %>% 
          `[`(2:length(.)) %>% 
          c('~', .)
      } else (
        #' Otherwise, this will be a relative directory movement. Add the
        #' target directory to the current directory.
        c(current_directory, dir_target)
      ) %>% 
        #' Then move up a directory for every '..' present
        handle_updir()
      
      #' Each time the current directory is updated, see if that directory,
      #' and the levels leading to it, are in the `file_system` list. If it
      #' doesn't exist, add it.
      if (
        length(current_directory) > 1 &
          is.null(Reduce(`[[`, current_directory %>% `[`(2:length(.)), init = dir_tree))
      ) {
        
        #' Recursively create new directories for each non-existent path
        dir_tree <- recursive_add(dir_tree, current_directory)
        
      }
      
    }
    
    #' If the command is `ls` then assign all files/directories found before
    #' the next command with a '$' at the start to the current directory
    if (commands[[i]] == paste(symbols$user_input, symbols$list_dir_contents)) {
      
      # PULL INTO SEPARATE FUNCTION
      
      remaining_console <- commands[(i + 1):length(commands)]
      
      #' Where is the next user input?
      next_user_input <- which(
        str_detect(remaining_console, paste0('^\\', symbols$user_input))
      ) %>% 
        min()
      
      #' This is the list of files and directories
      remaining_console[1:(next_user_input - 1)]
      
    }
    
  }
  
} 


command_crawler(system_commands, verbose = T)

# later, create a lookup table with file sizes and then use that to sum???

# or list(
#   [[1]] = table(filenames, sizes),
#   [[2]] = c('subdir1', 'subdir1')
# )
# First option might be easier to do the sum in - just unlist() the top level folder and lookup all filenames in table

Or list(
  
)