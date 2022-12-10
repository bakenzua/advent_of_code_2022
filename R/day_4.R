library(dplr)
library(tibble)

########################################
#         get inputs
########################################
sample_input_d4 <- tibble(
  x = str_split(
    "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8",
    "\n"
  ) |> unlist(),
)

input_4 <- adventdrob::advent_input(4, 2022)

########################################
#         do_day_4 function
########################################
do_day_4 <- function(data, part1 = TRUE) {
  data <- data |>
    mutate(
      a1 = str_extract(x, "[:digit:]*"),
      a2 = str_sub(str_extract(x, "-[:digit:]*"), 2),
      b1 = str_sub(str_extract(x, ",[:digit:]*"), 2),
      b2 = str_extract(x, "[:digit:]*$"),
      across(c("a1", "a2", "b1", "b2"), as.numeric),
      superset = ((a1 <= b1) & (a2 >= b2)) | ((a1 >= b1) & (a2 <= b2)),
      intersect = (a1 <= b2) & (a2 >= b1)
    )
  if (part1) {
    return(sum(data$superset))
  } else {
    return(sum(data$intersect))
  }
}

########################################
# look at the elf cleanup assignments
########################################
do_day_4(sample_input_d4)
do_day_4(input_4)

do_day_4(sample_input_d4, part1 = FALSE)
do_day_4(input_4, part1 = FALSE)
