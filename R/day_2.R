library(dplyr)
library(tibble)
library(stringr)

#######################################################
###           Get inputs
#######################################################
sample_input_d2 <- tibble(
  x = c("A Y", "B X", "C Z")
)
calc_score <- function(data) {
  df <- data |>
    mutate(
      a = str_sub(x, 1, 1),
      b = str_sub(x, 3),
      win = case_when(
        (a == "A" & b == "X") ~ 3, # rock, roc
        (a == "A" & b == "Y") ~ 6, # rock, paper
        (a == "A" & b == "Z") ~ 0, # rock, scissor

        (a == "B" & b == "X") ~ 0, # paper, roc
        (a == "B" & b == "Y") ~ 3, # paper, paper
        (a == "B" & b == "Z") ~ 6, # rock, scissor

        (a == "C" & b == "X") ~ 6, # scissors, roc
        (a == "C" & b == "Y") ~ 0, # scissors, paper
        (a == "C" & b == "Z") ~ 3, # scissors, scissor
      ),
      my_choice_score = case_when(
        b == "X" ~ 1,
        b == "Y" ~ 2,
        b == "Z" ~ 3,
      ),
      round_score = win + my_choice_score
    )
  return(sum(df$round_score))
}

calc_score_2 <- function(data) {
  df <- data |>
    mutate(
      a = str_sub(x, 1, 1),
      b = str_sub(x, 3),
      # select bb so strategy choice reuslts
      bb = case_when(
        (a == "A" & b == "X") ~ "Z", # rock, lose
        (a == "A" & b == "Y") ~ "X", # rock, draw
        (a == "A" & b == "Z") ~ "Y", # rock, win

        (a == "B" & b == "X") ~ "X", # paper, lose
        (a == "B" & b == "Y") ~ "Y", # paper, draw
        (a == "B" & b == "Z") ~ "Z", # rock, win

        (a == "C" & b == "X") ~ "Y", # scissors, lose
        (a == "C" & b == "Y") ~ "Z", # scissors, draw
        (a == "C" & b == "Z") ~ "X", # scissors, win
      ),
      win = case_when(
        (a == "A" & bb == "X") ~ 3, # rock, roc
        (a == "A" & bb == "Y") ~ 6, # rock, paper
        (a == "A" & bb == "Z") ~ 0, # rock, scissor

        (a == "B" & bb == "X") ~ 0, # paper, roc
        (a == "B" & bb == "Y") ~ 3, # paper, paper
        (a == "B" & bb == "Z") ~ 6, # paper, scissor

        (a == "C" & bb == "X") ~ 6, # scissors, roc
        (a == "C" & bb == "Y") ~ 0, # scissors, paper
        (a == "C" & bb == "Z") ~ 3, # scissors, scissor
      ),
      my_choice_score = case_when(
        bb == "X" ~ 1,
        bb == "Y" ~ 2,
        bb == "Z" ~ 3,
      ),
      round_score = win + my_choice_score
    )
  return(sum(df$round_score))
}
calc_score(sample_input_d2)
calc_score_2(sample_input_d2)

input <- adventdrob::advent_input(2, 2022)
calc_score(input)
calc_score_2(input)
