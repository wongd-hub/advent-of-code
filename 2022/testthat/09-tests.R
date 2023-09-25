# 09 Testing ----

#' #' Plotting steps
#' dat <- follow_h(
#'   origin_coords, 
#'   head_directions, 
#'   verbose = F
#' )$coord_tracker
#' 
#' for (i in 1:100) {
#'   
#'   print(
#'     dat %>% 
#'       slice(i) %>% 
#'       ggplot() +
#'       geom_point(aes(x = t_x, y = t_y), colour = 'red') +
#'       geom_point(aes(x = h_x, y = h_y), colour = 'blue') +
#'       coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
#'       labs(title = i)
#'   )
#' 
#'   #' Also plot the previous iteration's 
#'   readline()
#'     
#' }


## Unit Tests ----

test_that(
  'Moving T to H works properly',
  {
    
    #' Single axis moves
    expect_equal(
      move_t_to_h(list(h = c(x = 0, y = 2), t = c(x = 0, y = 0))),
      list(h = c(x = 0, y = 2), t = c(x = 0, y = 1))
    )
    expect_equal(
      move_t_to_h(list(h = c(x = 2, y = 0), t = c(x = 0, y = 0))),
      list(h = c(x = 2, y = 0), t = c(x = 1, y = 0))
    )
    expect_equal(
      move_t_to_h(list(h = c(x = 0, y = -2), t = c(x = 0, y = 0))),
      list(h = c(x = 0, y = -2), t = c(x = 0, y = -1))
    )
    expect_equal(
      move_t_to_h(list(h = c(x = -2, y = 0), t = c(x = 0, y = 0))),
      list(h = c(x = -2, y = 0), t = c(x = -1, y = 0))
    )
    
    expect_equal(
      move_t_to_h(list(h = c(x = 0, y = 3), t = c(x = 0, y = 0))),
      list(h = c(x = 0, y = 3), t = c(x = 0, y = 1))
    )
    expect_equal(
      move_t_to_h(list(h = c(x = -3, y = 0), t = c(x = 0, y = 0))),
      list(h = c(x = -3, y = 0), t = c(x = -1, y = 0))
    )
    
    
    #' Diagonal moves
    expect_equal(
      move_t_to_h(list(h = c(x = 2, y = 2), t = c(x = 0, y = 0))),
      list(h = c(x = 2, y = 2), t = c(x = 1, y = 1))
    )
    expect_equal(
      move_t_to_h(list(h = c(x = 2, y = -2), t = c(x = 0, y = 0))),
      list(h = c(x = 2, y = -2), t = c(x = 1, y = -1))
    )
    expect_equal(
      move_t_to_h(list(h = c(x = -2, y = -2), t = c(x = 0, y = 0))),
      list(h = c(x = -2, y = -2), t = c(x = -1, y = -1))
    )
    
  }
)

test_that(
  'Check follow_h() function',
  {
    
    test_instr <- c("D 1", "L 2", "D 2", "L 1", "R 1", "D 1", "R 2", "U 1", "R 1", "D 2")
    
    expect_equal(
      follow_h(origin_coords, test_instr, verbose = F)$coord_tracker %>% 
        distinct(t_x, t_y) %>% 
        nrow(),
      7
    )
    expect_equal(
      follow_h(origin_coords, test_instr, verbose = F)$coord_tracker %>% 
        nrow(),
      1 + sum(str_sub(test_instr, 3, 3) %>% as.numeric())
    )
    
    provided_instr <- c('R 4', 'U 4', 'L 3', 'D 1', 'R 4', 'D 1', 'L 5', 'R 2')
    
    expect_equal(
      follow_h(origin_coords, provided_instr, verbose = F)$coord_tracker %>% 
        distinct(t_x, t_y) %>% 
        nrow(),
      13
    )
    
  }
)
