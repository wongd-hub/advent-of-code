# 07 Testing ----

if (F) {
  
  #' Check that folder traversal works 
  system_snapshot(system_commands %>% str_subset('cd'), verbose = T)
  
  #' Check that adding directories found in `ls` works
  system_snapshot(system_commands %>% str_subset('cd|ls|dir') %>% `[`(1:100), verbose = T)
  
  #' Testing subset of all commands
  system_snapshot(system_commands[1:100], verbose = T)
  
}

## Unit tests ----

test_that(
  'handle_dir parses .. correctly',
  {
    
    #' Normal handling of '..'s
    expect_equal(handle_updir(c('~', 'a', '..')), c('~'))
    expect_equal(handle_updir(c('~', 'a', '..', 'b')), c('~', 'b'))
    expect_equal(handle_updir(c('~', 'a', '..', 'b', '..', 'c')), c('~', 'c'))
    expect_error(handle_updir(c('..')))
    
    #' Since we're parsing commands sequentially, there's no need for two '..'s
    #' in sequence to work as expected - we expect only the first '..' to be
    #' applied in this case``
    expect_equal(handle_updir(c('~', 'a', '..', '..')), c('~'))
    
  }
)

test_that(
  'recursive_add adds empty lists correctly',
  {
    
    #' Normal operations
    test_list <- list('~' = list())
    
    expect_equal(recursive_add(test_list, c('~', 'foo')), list('~' = list('foo' = list())))
    expect_equal(recursive_add(test_list, c('~', 'foo', 'bar')), list('~' = list('foo' = list('bar' = list()))))
    expect_equal(recursive_add(list(), c('~', 'foo')), list('~' = list('foo' = list())))
    
    #' Adding an empty path
    expect_equal(recursive_add(test_list, c()), test_list)
    
  }
)

test_that(
  'recursive_add adds files to directories correctly', 
  {
    
    #' Normal operations
    test_list <- list('~' = list())
    file_list <- c('file1' = 123, 'file2' = 456)
    
    expect_equal(recursive_add(test_list, c('~'), file_list), list('~' = list('file1' = 123 ,'file2' = 456)))
    expect_equal(recursive_add(test_list, c('~', 'foo'), file_list), list('~' = list('foo' = list('file1' = 123 ,'file2' = 456))))
    
  }
)

test_that(
  'handle_change_dir',
  {
    
  }
)

handle_list_dir

system_snapshot
