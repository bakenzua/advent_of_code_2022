############################################################################################
#  Day 1 inputs
############################################################################################
sample_input <- "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"
sample_input <- strsplit(sample_input, "\n") |> unlist()

input_1 <- adventdrob::advent_input(1, 2022)
############################################################################################
#  Day 1 function
############################################################################################
#' do_aoc_day1
#'
#' @param input
#' @param sample_text Run on sample text from question
#'
#' @return dataframe of Elf and the total calories in their inventory
#' @export
#'
#' @examples
do_aoc_day1 <- function(all_elf_foods) {
  all_elf_foods <- as.numeric(all_elf_foods)

  # logical vector of empty elements of all_elf_foods
  empty_all_elf_foods <- is.na(all_elf_foods)
  # split into individual elf inventories
  ind_elf_inventories <- unname(
    split(all_elf_foods[!empty_all_elf_foods], 
    cumsum(empty_all_elf_foods)[!empty_all_elf_foods])
  )

  # sum each elf inventory and unlist to vector
  ind_elf_total_calories <- lapply(ind_elf_inventories, sum) |> unlist()

  # to adatframe with elf id
  elf_dataframe <- data.frame(
    elf_id = seq_along(ind_elf_total_calories),
    ind_elf_total_calories = ind_elf_total_calories
  )
  # sort dataframe on calories
  elf_dataframe <- elf_dataframe[order(elf_dataframe$ind_elf_total_calories, decreasing = TRUE), ]

  return(elf_dataframe)
}

############################################################################################
#  Do sample data
############################################################################################
sample_input_result <- do_aoc_day1(sample_input)
# sample_input_result
sample_input_result |> head(1)
sum(sample_input_result[1:3, ]$ind_elf_total_calories)

############################################################################################
#  Do Day 1 !
############################################################################################
d1_result <- do_aoc_day1(d1_input)
# part 1
d1_result |> head(1)
# part 2
sum(d1_result[1:3, ]$ind_elf_total_calories)
