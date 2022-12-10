library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(tidyr)
#######################################################
###           Get inputs
#######################################################

sample_input_d5 <- tibble(
    x = str_split(
        "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2",
        "\n"
    ) |> unlist(),
)

input_5 <- adventdrob::advent_input(5, 2022)

#######################################################
###           Day 5 function
#######################################################

do_day_5 <- function(data, cargo_col_row, part_1 = TRUE) {
    # get all the crate stacks as vectors
    # This took ages and is not pretty
    cargo <- data |>
        slice_head(n = cargo_col_row - 1)
    column_n <- max(nchar(cargo$x))
    n_cols <- (column_n + 1) / 4
    cargo_split <- map(cargo$x, ~ str_split(.x, "")[[1]][seq(2, column_n - 1, by = 4)])
    cargo_stacks <- list()
    for (stack in 1:(n_cols + 1)) {
        cargo_stacks[stack] <- c()
    }
    for (stack in 1:n_cols) {
        for (layer in rev(seq_along(cargo_split))) {
            cargo_stacks[[stack]] <- c(
                cargo_stacks[[stack]],
                cargo_split[[layer]][stack]
            )
        }
    }
    # take off the empty spaces from the tops of the stacks
    cargo_stacks <- lapply(cargo_stacks, function(x) x[x != " "])

    # get the move rows
    move_rows <- data |>
        filter(str_detect(x, "move")) |>
        # use one extract rather than multiple regexp
        extract(
            x,
            c("n", "from_col", "to_col"),
            "move (\\d+) from (\\d+) to (\\d+)"
        ) |>
        mutate(
            across(1:3, as.numeric)
        )
    for (m in seq_len(nrow(move_rows))) {
        # get move instructions
        move_row <- move_rows[m, ]
        # get the crates to move depending on crane version!
        if (part_1) {
            crates_to_move <- rev(tail(cargo_stacks[[move_row$from_col]], move_row$n))
        } else {
            crates_to_move <- tail(cargo_stacks[[move_row$from_col]], move_row$n)
        }

        # take them off the stack from_col
        cargo_stacks[[move_row$from_col]] <- head(cargo_stacks[[move_row$from_col]], -move_row$n)
        # add them to the to_col stack
        cargo_stacks[[move_row$to_col]] <- c(cargo_stacks[[move_row$to_col]], crates_to_move)
    }

    top_crates <- c()
    for (i in seq_along(cargo_stacks)) {
        top_crates <- c(top_crates, tail(cargo_stacks[[i]], 1))
    }
    return(paste0(top_crates, collapse = ""))
}

#######################################################
###           Do the restackings
#######################################################
do_day_5(sample_input_d5, 4) == "CMZ" # "CMZ"
do_day_5(input_5, 9) # "FWNSHLDNZ"
do_day_5(input_5, 9, part_1 = FALSE) # "RNRGDNFQG"
