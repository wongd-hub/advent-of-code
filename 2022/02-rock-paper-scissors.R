# 02 Rock Paper Scissors ----
#' https://adventofcode.com/2022/day/2

#' The elves are now playing Rock Paper Scissors. They provide you with a
#' strategy guide.
#'
#' - Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock
#' - In the strategy guide, the first column tells you what your opponent plays, 
#'   and the second column is your response
#' - For the opponent, A: Rock, B: Paper, C: Scissors
#' - For you, X: Rock, Y: Paper, Z: Scissors
#' - The score at each round is the sum of the score for the shape you selected 
#'   (1: Rock, 2: Paper, 3: Scissors), and the score for your outcome (0: Loss,
#'   3: Draw, 6: Win)
#' 
#' Sum your total scores across the entire strategy guide.

library(tidyverse)

## 02a Part 1 ----

#' Raw text file => one strategy per line character vector
day_2_input_fp <- max(list.files(file.path('2022', 'inputs'), 'day_2', full.names = TRUE))
rps_strategy_guide <- readLines(day_2_input_fp)

#' Define functions + helpers to manipulate strategy data and score

#' Convert strategy to table
#' 
#' @params .strategy, .strategy_tbl arguments passed from `rps_strategy_score()`
get_strategy_table <- function(.strategy, .strategy_tbl) {
  
  #' If no strategy table is provided, convert `strategy` to a table...
  if (is.null(.strategy_tbl)) {
    
    #' ... where the first column is the opponent's plays, and the second 
    #' is yours
    strategy_lst <- .strategy %>% 
      str_extract_all('[A-Z]') %>% 
      transpose() %>% 
      map(unlist) %>% 
      set_names(c('opponent', 'you'))
    
    .strategy_tbl <- bind_cols(strategy_lst)
    
  }
  
  return (.strategy_tbl)
  
}

#' Get round score
#' 
#' Determines the outcome of a round from player 1's perspective
#' 
#' NB: Could programmatically create this from an arbitrary set of rules
#' passed to the main function, but since the rules are relatively simple,
#' we'll keep it like this for now.
#' 
#' @params player_1, player_2 Two vectors containing 'Rock'/'Paper'/'Scissors'
#'   for each round
#' @params .round_scores A named vector of outcomes, names should be 'Win',
#'   'Draw', and 'Lose'
rps_round_score <- function(player_1, player_2, .round_scores) case_when(
  (player_1 == 'Rock' & player_2 == 'Scissors') |
    (player_1 == 'Scissors' & player_2 == 'Paper') |
    (player_1 == 'Paper' & player_2 == 'Rock') ~ .round_scores[['Win']],
  player_1 == player_2                         ~ .round_scores[['Draw']],
  is.na(player_1) | is.na(player_2)            ~ NA,
  TRUE                                         ~ .round_scores[['Lose']]
)

#' Convert A-C/X-Z to Rock, Paper, or Scissors
#' 
#' @param letters A vector containing A-C/X-Z to convert to R-S
convert_to_rps <- function(letters) case_when(
  #' If already converted, don't do anything
  letters %in% 
    c('Rock', 'Paper', 'Scissors') ~ letters,
  #' Else, convert
  letters %in% c('A', 'X') ~ 'Rock',
  letters %in% c('B', 'Y') ~ 'Paper',
  letters %in% c('C', 'Z') ~ 'Scissors',
  TRUE                     ~ NA_character_
)

#' Score a strategy
#' 
#' @param strategy A character string where each element represents the opponent
#'   and your plays, space delimited e.g. c("B Z", "A X")
#' @param strategy_tbl Optionally a table with two columns, your opponent's play
#'   and your play. Names should be 'opponent' and 'you'.
#' @param choice_scores
#' @param round_scores
#' 
#' @example 
#' rps_strategy_score(c('A Y')) %>% 
#'   # Calculate your total score
#'   mutate(you_total_score = you_shape_score + you_round_score)
rps_strategy_score <- function(
  strategy      = NULL, 
  strategy_tbl  = NULL,
  shape_scores  = c('Rock' = 1, 'Paper' = 2, 'Scissors' = 3),
  round_scores  = c('Win' = 6, 'Draw' = 3, 'Lose' = 0)
) {
  
  # Set up strategy table ----
  strategy_tbl <- get_strategy_table(.strategy = strategy, .strategy_tbl = strategy_tbl)

  # Score strategy ----
  
  #' Note that Rock > Scissors > Paper, but Paper > Rock, so the outcomes are
  #' not transitive and we can't handle this by converting the A/B/C X/Y/Z
  #' values into a numerical factor and working off inequalities.
  #' 
  #' Need to codify scoring rules some other way, will use case_whens here
  #' 
  #' Total score is a sum of shape score (which option did you choose) and
  #' round score (whether your choice beats your opponent's choice)

  #' Get shape score // could have also joined a lookup table onto `strategy_tbl`
  rps_shape_score <- function(choices, .shape_scores = shape_scores) case_when(
    choices %in% c('Rock')     ~ .shape_scores[['Rock']],
    choices %in% c('Paper')    ~ .shape_scores[['Paper']],
    choices %in% c('Scissors') ~ .shape_scores[['Scissors']],
    TRUE                       ~ NA_integer_
  )
  
  #' Score and return
  strategy_tbl %>% 
    mutate(
      across(c(opponent, you), convert_to_rps),
      across(c(opponent, you), rps_shape_score, .names = '{.col}_shape_score'),
      you_round_score      = rps_round_score(you, opponent, .round_scores = round_scores),
      opponent_round_score = rps_round_score(opponent, you, .round_scores = round_scores)
    )
  
}

#' Calculating your total score
rps_strategy_score(rps_strategy_guide) %>% 
  mutate(you_total_score = you_shape_score + you_round_score) %>% 
  .$you_total_score %>% 
  sum()

#' Answer is [11666]

## 02b Part 2 ----

#' The elves now clarify that the second column indicates what outcome you need
#' to achieve (X: you need to lose, Y: you need to draw, Z: you need to win).
#' 
#' - Total score is counted in the same way
#' - You need to figure out which shape to choose so the round ends as indicated


#' Write a func to determine which shape to use, then run `rps_strategy_score()`
#' on the resulting tibble

#' Determine the required shape to get the desired outcome
#'
#' @param strategy A character string where each element represents the
#'   opponent's play and the desired outcome, space delimited e.g. c("B Z", "A X")
#' @param strategy_tbl Optionally a table with two columns, the opponent's play
#'   and the desired outcome. Names should be 'opponent' and 'outcome'.
#' @param keep_outcome Return the outcome column as well as the play that you're
#'   determined to run in each round
rps_get_required_shape <- function(
  strategy      = NULL, 
  strategy_tbl  = NULL,
  keep_outcome  = FALSE,
  round_outcome = c('Win' = 'Win', 'Lose' = 'Lose', 'Draw' = 'Draw')
) {
  
  # Set up strategy table ----
  strategy_tbl <- get_strategy_table(.strategy = strategy, .strategy_tbl = strategy_tbl) %>% 
    rename(outcome = you)
  
  # Find required shape ----
  
  convert_outcome <- function(letters) case_when(
    letters %in% c('X') ~ 'Lose',
    letters %in% c('Y') ~ 'Draw',
    letters %in% c('Z') ~ 'Win',
    TRUE                ~ NA_character_
  )
  
  #' Generate table of options in the same fashion as the previous function;
  #' just turning the logic around to reuse code from the first part (opponent +
  #' your play => outcome, as opposed to opponent + outcome => your play)
  rps_all_play_scenarios <- crossing(
    opponent = c('Rock', 'Paper', 'Scissors'),
    you = c('Rock', 'Paper', 'Scissors')
  ) %>% 
    mutate(outcome = rps_round_score(you, opponent, .round_scores = round_outcome))

  #' Return output
  strategy_tbl %>% 
    mutate(
      opponent = convert_to_rps(opponent),
      outcome  = convert_outcome(outcome)
    ) %>% 
    left_join(rps_all_play_scenarios, by = c('opponent', 'outcome')) %>% 
    {
      if (keep_outcome) select(., opponent, you, outcome) else select(., opponent, you)
    }
  
}

#' Figure out what your plays will be - and therefore what score in total you will get
rps_get_required_shape(rps_strategy_guide, keep_outcome = T) %>% 
  rps_strategy_score(strategy_tbl = .) %>% 
  # # Checking to see if everything's applied correctly
  # filter(
  #   (outcome == 'Win' & you_round_score != 6) | 
  #     (outcome == 'Lose' & you_round_score != 0) | 
  #     (outcome == 'Draw' & you_round_score != 3)
  # ) %>% 
  # nrow == 0 # TRUE
  # Calculating your total score
  mutate(you_total_score = you_shape_score + you_round_score) %>% 
  .$you_total_score %>% 
  sum()

#' The answer is [12767]