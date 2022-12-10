library(dplyr)
library(tibble)
library(stringr)

#######################################################
###           Get inputs
#######################################################

sample_input_d3 <- tibble(
  x = str_split(
    "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw",
    "\n"
  ) |> unlist()
)

input <- adventdrob::advent_input(3, 2022)

#######################################################
###           do_rucksack_priorities function
#######################################################

do_rucksack_priorities <- function(data) {
  detect_common_str <- function(a, b) {
    intersect(str_split(a, "")[[1]], str_split(b, "")[[1]])
  }
  all_letters <- c(letters, LETTERS)

  data <- data |>
    mutate(
      l = str_count(x) / 2,
      com_str = map2_chr(str_sub(x, 1, l), str_sub(x, l + 1, 2 * l), detect_common_str),
      com_str_score = map_int(com_str, ~ which(all_letters == .x))
    )
  return(sum(data$com_str_score))
}


#######################################################
###           do_rucksack_priorities2 function
###                 for part 2
#######################################################
do_rucksack_priorities2 <- function(data) {
  detect_common_str3 <- function(one, two, three) {
    intersect(
      str_split(one, "")[[1]],
      intersect(
        str_split(two, "")[[1]],
        str_split(three, "")[[1]]
      )
    )
  }
  all_letters <- c(letters, LETTERS)

  data$id <- rep(1:(nrow(data) / 3), each = 3)
  data$name <- rep(c("elf1", "elf2", "elf3"), times = (nrow(data) / 3))
  data <- data |>
    pivot_wider(id, names_from = name, values_from = x) |>
    mutate(
      common_str = pmap_chr(list(elf1, elf2, elf3), detect_common_str3),
      com_str_score = map_int(common_str, ~ which(all_letters == .x))
    )
  return(sum(data$com_str_score))
}

#######################################################
###           do part 1
#######################################################
sample_input_d3 |>
  do_rucksack_priorities()

input |>
  do_rucksack_priorities()

#######################################################
###           do part 2
#######################################################
# check sample data solution
sample_input_d3 |>
  do_rucksack_priorities2()
# 70

input |>
  do_rucksack_priorities2()
# 2545
