# 07 No Space Left On Device ----
#' https://adventofcode.com/2022/day/7

#' You are trying to update the device but have run out of space. You decide to
#' browse the file system to see what you can delete to make more room.
#'
#' - The file system is composed of files and directories, the outermost directory is /
#' - Console commands are the same as bash - `cd` and `ls` are the main functions used
#'   - `dir xyz` means there exists a directory called `xyz` in the current folder
#'   - `[num] xyz` means there exist a file in the current directory with size [num]
#'   - Commands input by the user start with a `$`
#'
#' The goal is to get the size of each directory (at every level). For the first
#' part of the question, get the size of all directories at most 100k in size and
#' sum them (allowing for double-counting files).
#' 
#' Assume directories don't have any inherent size themselves.

source(file.path('2022', 'utils', 'libs_and_funs.R'))


## 07a Part 1 ----
### Data ingestion ----
#' Raw text file - use empty line to delimit the crate drawing and the instructions
system_commands <- get_input(2022, 7)

#' Two things we need to do are:
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

#' An alternative to this (possibly easier to handle, and easier to understand) 
#' is to use nested tibbles to represent the file structure; e.g.:
#'
#'  file_path_until_current_dir current_dir                           files
#'                c('~', 'foo')       'baz' c('file1' = 123, 'subdir1' = 0)
#'         c('~', 'foo', 'baz')   'subdir1'                c('file2' = 789)
#'
#' This means we wouldn't need recursive functions to add new subdirs, just
#' `add_row()` to this table. Then to get the total size of folder 'foo', filter
#' for file paths: 
#'  - That contain `current_dir` == 'foo' and `file_path_until_current_dir` == c('~')
#'  - And; with `file_path_until_current_dir` == c('~', 'foo') to get subdirs
 

### Revealing the file system ----
#### Function definitions ----

#' Handle '..'s in filepath
#'
#' We need to be able to handle '..'s which mean to move up one directory level.
#' We won't handle this directly in the `case_when()` because of the complexity
#' if this were passed in as part of a larger command (e.g. "hello/../world").
#' Instead, we'll check the current directory for '..'s every time it's updated
#' and remove them + the elements immediately preceding them to simulate moving
#' up one dir level.
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

#' Handle a 'change directory' command
#'
#' This function will parse a `cd` command to: 
#'   1. Find what folder we want to traverse to in relation to the 
#'      current directory, change the `current_directory` to this
#'   2. Add that folder to the `directory_index` and add an empty 
#'      sub-folder to the `dir_tree` if it doesn't already exist
#'
#' @param command_idx An integer representing which command to action
#' @param data_object List passed in by `system_snapshot()`
handle_change_dir <- function(
    command_idx,
    data_object
) {
  
  #' Get directory we're changing to
  dir_target <- str_remove(
    data_object$command[[command_idx]], 
    fixed(paste0(data_object$symbols$user_input, ' ', data_object$symbols$change_dir, ' '))
  )
  
  #' Find where we are in the directory tree
  #'   Can't use `case_when()` since it recycles RHS to the largest RHS
  #'   output supplied. Use if-else instead.
  data_object$current_directory <- if (
    #' If we're navigating to the home directory - move us back there
    dir_target == data_object$symbols$dir_sep
  ) {
    '~'
  } else if (
    #' Else if the path is an absolute path (starts with a '/') then build
    #' the full path, splitting by '/'
    str_sub(dir_target, 1, 1) == data_object$symbols$dir_sep & nchar(dir_target) > 1
  ) {
    str_split_1(dir_target, '/') %>% `[`(2:length(.)) %>% c('~', .)
  } else {
    #' Otherwise, this will be a relative directory movement. Add the
    #' target directory to the current directory.
    c(data_object$current_directory, dir_target)
  } %>% 
    #' Then move up a directory for every '..' present
    handle_updir(updir_sym = data_object$symbols$updir)
  
  #' Each time the current directory is updated, see if that directory,
  #' and the levels leading to it, are in the `file_system` list. If it
  #' doesn't exist, add it.
  if (
    length(data_object$current_directory) > 1
  ) {
    
    data_object$directory_index <- data_object$directory_index %>% 
      add_row(directory = list(data_object$current_directory))
    
    #' If doesn't exist in tree, add it
    if (is.null(Reduce(`[[`, data_object$current_directory, init = data_object$dir_tree))) {
      
      #' Recursively create new directories for each non-existent path
      data_object$dir_tree <- recursive_add(data_object$dir_tree, data_object$current_directory)
      
    }
    
  }
  
  data_object
  
}

#' Handle a 'list directory' command
#' 
#' This function parses a `ls` command to:
#'   1. Find the relevant output of this `ls` command, extract files 
#'      and directories from it
#'   2. For directories, add to the `directory_index` and `dir_tree` 
#'      if they don't already exist there
#'   3. For files, add them to the relevant directory as a named numeric 
#'      vector, where the vector item represents the file's size
#' 
#' @param command_idx An integer representing which command to action
#' @param data_object List passed in by `system_snapshot()`
handle_list_dir <- function(
  command_idx,
  data_object
) {
  
  #' Get the remaining console commands/outputs
  remaining_console <- data_object$commands[(command_idx + 1):length(data_object$commands)]
  
  #' Find the next user input in the remaining console output
  suppressWarnings(
    next_user_input <- which(
      str_detect(remaining_console, paste0('^', '\\', data_object$symbols$user_input))
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
    str_subset(fixed(paste0(data_object$symbols$dir_prefix, ' '))) %>% 
    str_remove(fixed(paste0(data_object$symbols$dir_prefix, ' ')))
  
  file_console_output <- cd_list %>% 
    str_subset(paste0('^', data_object$symbols$file_prefix, ' '))
  
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
      
      #' Set current directory temporarily to this subfolder
      tmp_current_directory <- c(data_object$current_directory, .x)
      
      #' Add to directory index
      data_object$directory_index <- data_object$directory_index %>% 
        add_row(directory = list(tmp_current_directory))
      
      #' Modify the directory tree if this subdirectory doesn't exist in it
      data_object$dir_tree <- recursive_add(data_object$dir_tree, tmp_current_directory)
      
    })
  
  
  #' Handling files
  #'  Add files to the last directory in `current_directory`
  data_object$dir_tree <- recursive_add(data_object$dir_tree, data_object$current_directory, files = file_list)
  
  data_object
  
}

#' Create a system snapshot and calculate size of each folder
#'
#' Loops over all system commands/outputs and parses them to create a snapshot
#' of the directory structure (`dir_tree`) and a table with the size of each
#' folder (`dir_size`, which is derived from `directory_index`).
#'
#' @param commands A character vector containing the commands/outputs
#' @param symbols A list containing all relevant command line symbols
#' @param verbose A boolean - whether to print logs to console
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

  #' Data payload, this will contain data structures that will be edited as each
  #' command is executed.
  .data <- list(
    
    #' Data structures
    #'  Representations of the directory tree and the current
    #'  working directory
    dir_tree          = list(),
    current_directory = '~',
    directory_index   = tibble(directory = list('~')),
    
    #' User input
    commands          = commands,
    symbols           = symbols
    
  )

  #' Loop over all commands and action them
  for (i in 1:length(.data$commands)) {
    
    #' Log to console if `verbose`
    if (verbose) {
      cat('Current directory:', .data$current_directory, '\n')
      cat('Parsing command', i, '-', commands[[i]], '\n\n')
    }
    
    
    #' Handle change directory/list directory commands
    if ( #' If the command is to change directory...
      str_detect(.data$commands[[i]], fixed(paste(.data$symbols$user_input, .data$symbols$change_dir)))
    ) {

      #' If the command is `cd`, then switch `current_directory` and check to
      #' see if that directory is in the `dir_tree` yet. If not, add it.
      #'
      #' NB: 
      #'  `cd`s without a '/' in front of the path are [relative], whereas
      #'  those that start with '/' are [absolute] from the perspective of the
      #'  home directory
      .data <- handle_change_dir(command_idx = i, data_object = .data)
      
    } else if ( #' If the command is to list directory contents...
      .data$commands[[i]] == paste(.data$symbols$user_input, .data$symbols$list_dir_contents)
    ) {
      
      #' If the command is `ls` then assign all files/directories found before
      #' the next command with a '$' at the start to the current directory
      .data <- handle_list_dir(command_idx = i, data_object = .data)
      
    }
    
  }

  #' Once done, go into each part of the tree, un-list, and sum to get total size.
  dir_sizes <- .data$directory_index %>% 
    distinct(directory) %>% 
    pmap_dfr(~{
      
      #' Traverse to the required point of the tree
      working_dir <- Reduce(`[[`, .x, init = .data$dir_tree)
      
      tibble(
        directory = list(.x),
        dir_string = paste(.x, collapse = '/'),
        #' Flatten structure to sum all files under that point in the tree
        dir_size = working_dir %>% unlist() %>% sum()
      )
      
    })
  
  #' Return final objects
  list(
    dir_tree  = .data$dir_tree,
    dir_sizes = dir_sizes 
  )
  
}


### Testing / unit tests ----

source(file.path('2022', 'testthat', '07-tests.R'))


### Answer ----
system_model <- system_snapshot(system_commands)

system_model$dir_sizes %>% 
  # arrange(desc(dir_size)) %>% 
  filter(dir_size <= 100000) %>% 
  .$dir_size %>% sum()

#' Answer is [1581595]


## 07b Part 2 ----

#' We now want to clear enough space on the system 
#'
#'  - We have 70000000 of space
#'  - We need 30000000 of space to conduct an update
#'
#' What is the smallest directory we can delete that will give us enough space
#' to perform this update? What is the size of that directory?

#' Parameters
total_space  <- 70000000
space_needed <- 30000000
space_taken  <- system_model$dir_sizes %>% filter(dir_string == '~') %>% pull(dir_size)

#' Calculation
space_available <- total_space - space_taken
need_to_clear   <- space_needed - space_available

#' Which folder has the smallest size above `need_to_clear`?
system_model$dir_sizes %>% 
  filter(dir_size >= need_to_clear) %>% 
  .$dir_size %>% 
  min()

#' The answer is [1544176]