# 10 Cathode-Ray Tube ----

#' You need to design a replacement for your handheld device's video system. It
#' is made of a cathode-ray tube and works on [clock cycles] and a [register (X)] 
#' that holds only one number at a time. X starts at 1.
#' 
#' There are only two instructions the CPU can take:
#'  - `addx V` - this takes two cycles to complete and adds V to the CPU register
#'      i.e. if we start with `addx 3`, X remains 1 throughout cycles 1 & 2,
#'      then at the end of 2 (start of 3), X will become 4.
#'  - `noop` - this takes one cycle to complete and has no other effect
#'  
#' We are interested in the [signal strength], the cycle number multiplied by
#' the register value at that time. Find the signal strength at cycle 20 and
#' every 40 cycles after that (i.e. 60, 100), then sum them to find the answer.

source(file.path('2022', 'utils', 'libs_and_funs.R'))

## 10a Part 1 ----
### Data ingestion ----

#' Raw text file
cpu_instructions <- get_input(2022, 10)


### Function definitions ----

#' Parse a CPU instruction
#'
#' @param cpu_instr A string representing the command and optional argument that
#'   make up the instruction
parse_cpu_instr <- function(cpu_instr) c(
  'command' = str_extract(cpu_instr, '^[a-z]*'),
  'value'   = str_extract(cpu_instr, '-?[0-9]*$') %>% na_if('')
)

#' Execute a list of CPU instructions
#' 
#' @param cpu_instructions A character vector containing the list of instructions
execute_cpu_instructions <- function(cpu_instructions) {
  
  #' Set up iterators
  clock_cycle  <- 1
  cpu_register <- 1
  
  #' Create results collector and provide a default value
  result_list <- vector('list', length(cpu_instructions) + 1)
  result_list[[1]] <- tibble(cycle = 1, register = 1)
  
  #' Execute instructions, tracking the register at each cycle
  for (i in seq_along(cpu_instructions)) {
    
    #' Parse instructions, then decide what to do based on the command
    instr_line <- parse_cpu_instr(cpu_instructions[[i]])

    if (instr_line[['command']] == 'noop') {
      
      #' Advance time and otherwise do nothing
      clock_cycle <- clock_cycle + 1
      
    } else if (instr_line[['command']] == 'addx') {
      
      #' Advance time and set new register
      clock_cycle  <- clock_cycle + 2
      cpu_register <- cpu_register + as.numeric(instr_line[['value']])
      
    }
    
    #' Track register changes across clock cycles
    result_list[[i + 1]] <- tibble(cycle = clock_cycle, register = cpu_register)
    
  }
  
  #' The list of cycles will be incomplete because some instructions need more
  #' than one cycle to complete. Complete this list and interpolate the register
  #' value in missing cycles.
  tmp_result_df <- bind_rows(result_list)
  
  tibble(cycle = 1:max(tmp_result_df$cycle)) %>% 
    left_join(tmp_result_df, by = 'cycle') %>% 
    fill(register, .direction = 'down')
  
}

#' Get cycles of interest, calculate signal strength, then sum
cycle_register_values <- execute_cpu_instructions(cpu_instructions)

cycle_register_values %>% 
  filter(cycle %in% seq(from = 20, to = max(.$cycle), by = 40)) %>% 
  mutate(signal_strength = cycle * register) %>% 
  .$signal_strength %>% sum()

#' Answer is [15020]

## 10b Part 2 ----

#' Turns out the cpu register (X) is communicating the position of a sprite on
#' the cathode-ray tube screen. Each clock cycle corresponds to a pixel on the
#' 40 wide x 6 high screen.
#'
#' The sprite is 3 pixels long and X communicates the position of the middle of
#' that sprite. If, at any clock cycle, the clock cycle number matches up with
#' any 3 of the sprite's pixels, a lit pixel (#) is drawn. Otherwise the pixel
#' is left dark (.).
#'
#' Render the image that the instructions provided in Part 1 yields. What
#' capital letters appear?

pixels <- cycle_register_values %>% 
  slice(1:240) %>% 
  mutate(
    #' X coord - each row has 40 pixels
    x_coord = rep(0:39, 240 / 40),
    #' Y coord - which row is this on
    y_coord = ((cycle - 1) %/% 40) + 1,
    #' Upper/lower bound based on sprite being 3 pixels wide
    lower_bound = register - 1, 
    upper_bound = register + 1,
    #' Determining whether the pixel is lit
    pixel = ifelse(x_coord >= lower_bound & x_coord <= upper_bound, '#', '.')
  )

#' Render
pixels %>% 
  group_by(y_coord) %>% 
  group_split() %>% 
  map(~{.x$pixel %>% paste(collapse = '')}) %>% 
  paste(collapse = '\n') %>% 
  cat()

#> ####.####.#..#..##..#....###...##..###..
#> #....#....#..#.#..#.#....#..#.#..#.#..#.
#> ###..###..#..#.#....#....#..#.#..#.#..#.
#> #....#....#..#.#.##.#....###..####.###..
#> #....#....#..#.#..#.#....#....#..#.#....
#> ####.#.....##...###.####.#....#..#.#....

#' Answer is [EFUGLPAP]