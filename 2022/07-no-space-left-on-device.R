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

source(file.path('2022', 'utils', 'libs_and_funs.R'))

## 07a Part 1 ----
### Data ingestion ----
#' Raw text file - use empty line to delimit the crate drawing and the instructions.
system_commands <- get_input(2022, 7)

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
#  For every ls, populate the current working folder with the contents (dirs and files)


#### Function Definitions ----

#' Handle '..'s in filepath
#'
#' We need to be able to handle '..'s which mean to move up one directory level.
#' We won't handle this directly in the `case_when()` because of the complexity
#' if this were passed in as part of a larger command (e.g. "hello/../world").
#' Instead, we'll check the current directory for '..'s every time it's updated
#' and remove them + the elements immediately preceding them to simulate moving
#' up one dir lvl.
#'
#' @param directory A character vector representing the current directory, each
#'   element being a directory level
handle_updir <- function(string, updir_sym = '..') {
  
  #' Get location of '..'s and also the directories one level above
  updir_markers <- which(string == updir_sym)
  
  if (length(updir_markers) > 0) {
    
    prev_dir <- updir_markers - 1
    
    to_remove <- c(prev_dir, updir_markers)
    if (any(to_remove < 1)) stop('Internal error: an ".." is attempting to go above the home directory level')
    
    string[-to_remove]
    
  } else string
  
}

#' Recursively traverse list and add empty list and/or file list
#'
#' We need a way to traverse a nested list and add new empty lists at any
#' arbitrary level.
#'
#' @param list A list to recursively modify
#' @param path A character vector containing the directory levels to check
#' @param files A character vector containing files to add to the directory
#'   selected in `path`
recursive_add <- function(list, path, files = NULL) {
  
  # If the path is empty, return the list - our success condition
  if (length(path) == 0) return(list)
  
  # Split the path into the current directory and the rest
  target_dir <- path[1]
  rest <- path[-1]
  
  # If the current part doesn't exist in the list, add it
  if (!target_dir %in% names(list)) list[[target_dir]] <- list()
  
  # If the vector of files to add is non-NULL, and we're on the last directory
  # in the path, append the files to the list
  if (!is.null(files) & length(path) == 1) list[[target_dir]] <- list[[target_dir]] %>% append(files)
  
  # Walk down the list, and recursively modify the next level
  list[[target_dir]] <- recursive_add(list[[target_dir]], rest, files = files)
  
  return(list)
  
}


system_snapshot <- function(
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
  directory_index <- tibble(directory = list(current_directory))

  #' Loop over all commands and action them
  for (i in 1:length(commands)) {
    
    if (verbose) {
      cat('Current directory:', current_directory, '\n')
      cat('Parsing command', i, '-', commands[[i]], '\n\n')
    }
    
    #' If the command is to change directory...
    if (str_detect(commands[[i]], fixed(paste(symbols$user_input, symbols$change_dir)))) {
      
      # PULL INTO SEPARATE FUNCTION?
      
      #' Get directory we're changing to
      dir_target <- str_remove(commands[[i]], fixed(paste0(symbols$user_input, ' ', symbols$change_dir, ' ')))
      
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
        str_split_1(dir_target, '/') %>% `[`(2:length(.)) %>% c('~', .)
      } else {
        #' Otherwise, this will be a relative directory movement. Add the
        #' target directory to the current directory.
        c(current_directory, dir_target)
      } %>% 
        #' Then move up a directory for every '..' present
        handle_updir(updir_sym = symbols$updir)
      
      #' Each time the current directory is updated, see if that directory,
      #' and the levels leading to it, are in the `file_system` list. If it
      #' doesn't exist, add it.
      if (
        length(current_directory) > 1
      ) {
        
        directory_index <- directory_index %>% 
          add_row(directory = list(current_directory))
        
        
        #' If doesn't exist in tree, add it
        if (is.null(Reduce(`[[`, current_directory, init = dir_tree))) {
          
          #' Recursively create new directories for each non-existent path
          dir_tree <- recursive_add(dir_tree, current_directory)
          
        }

      }
      
    }
    
    #' If the command is `ls` then assign all files/directories found before
    #' the next command with a '$' at the start to the current directory
    #' 
    #' NB: Technically, if we don't explore these folders, we don't know what's
    #' in them and therefore can't enumerate the overall size. However, we'll
    #' add this code anyway to make sure we get a full view of the directory tree
    if (commands[[i]] == paste(symbols$user_input, symbols$list_dir_contents)) {

      remaining_console <- commands[(i + 1):length(commands)]

      #' Where is the next user input?
      suppressWarnings(
        next_user_input <- which(
          str_detect(remaining_console, paste0('^', '\\', symbols$user_input))
        ) %>%
          min()
      )
      
      cd_list <- if (length(next_user_input) > 0 & !is.infinite(next_user_input)) {
        
        #' If there are more commands later on, only grab up until the next command
        #'  This is the list of files and directories before the next `cd`
        remaining_console[1:(next_user_input - 1)]
        
      } else if (length(next_user_input) == 1 & is.infinite(next_user_input)) {
        
        #' Otherwise, if there are no more commands, just grab up to the end of the command list
        remaining_console[1:length(remaining_console)]
        
      }
        
      #' Extracting listed files and directories
      dirs <- cd_list %>% 
        str_subset(fixed(paste0(symbols$dir_prefix, ' '))) %>% 
        str_remove(fixed(paste0(symbols$dir_prefix, ' ')))

      file_console_output <- cd_list %>% 
        str_subset(paste0('^', symbols$file_prefix, ' '))
      
      file_list <- file_console_output %>% 
        str_extract('^[0-9]+') %>% 
        as.numeric() %>% 
        setNames(
          file_console_output %>% 
            str_remove('^[0-9]+ ')
        )
      
      #' Handling directories
      #'  Loop over directories and add them to the tree
      dirs %>% 
        walk(~{
          
          # print(paste('Adding directory', .x, 'to', paste(current_directory, collapse = '/')))
          
          #' Set current directory temporarily to this subfolder
          tmp_current_directory <- c(current_directory, .x)
          
          #' Add to directory index
          directory_index <<- directory_index %>% 
            add_row(directory = list(tmp_current_directory))
          
          # print(dir_tree)
          
          #' Modify the directory tree if this subdirectory doesn't exist in it
          dir_tree <<- recursive_add(dir_tree, tmp_current_directory)
          
          # print('Added...')
          # print(dir_tree)
          
        })
      
      
      #' Handling files
      #'  Add files to the last directory in `current_directory`
      dir_tree <- recursive_add(dir_tree, current_directory, files = file_list)
      # append them separately or in their own character vector - either way you can just use unlist() at the end of it all

    }
    
  }

  #' Once done, go into each part of the tree, unlist, and sum to get total size.
  dir_sizes <- directory_index %>% 
    distinct(directory) %>% 
    pmap_dfr(~{
      
      working_dir <- Reduce(`[[`, .x, init = dir_tree)
      
      tibble(
        directory = list(.x),
        dir_string = paste(.x, collapse = '/'),
        dir_size = working_dir %>% unlist() %>% sum()
      )
      
    })
  
  
  list(
    dir_tree  = dir_tree,
    dir_sizes = dir_sizes 
  )
  
} 

#' Testing 
if (F) {
  
  #' Check that folder traversal works 
  system_snapshot(system_commands %>% str_subset('cd'), verbose = T)
  
  #' Check that adding directories found in `ls` works
  system_snapshot(system_commands %>% str_subset('cd|ls|dir') %>% `[`(1:100), verbose = T)
  
  #' Testing subset of all commands
  system_snapshot(system_commands[1:100], verbose = T)
  
}

#' Answer
system_model <- system_snapshot(system_commands)

system_model$dir_sizes %>% 
  # arrange(desc(dir_size)) %>% 
  filter(dir_size <= 100000) %>% 
  .$dir_size %>% sum()

#' Answer is [1581595]