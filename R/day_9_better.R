library(dplyr)
library(stringr)

#######################################################
###    An improved solution using vectors for 
###    flow control logic, positions and movement etc.
###    Tibbles etc reserved for the once only stuff
###    case_when rather than sequential if statements 
#######################################################

#######################################################
###           Get inputs
#######################################################

sample_input_d9 <- tibble(
    x = str_split(
        "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2",
        "\n"
    ) |> unlist()
)
sample_input_2_d9 <- tibble(
    x = str_split(
        "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20",
        "\n"
    ) |> unlist()
)

input_9 <- adventdrob::advent_input(9, 2022)

#######################################################
###          functions
#######################################################

move_head <- function(direction, pos) {
    case_when(
        direction == "L" ~ pos + c(-1, 0),
        direction == "R" ~ pos + c(1, 0),
        direction == "U" ~ pos + c(0, 1),
        direction == "D" ~ pos + c(0, -1)
    )
}
calc_new_knot_position <- function(knot1, knot2) {
    if (max(abs(knot1 - knot2)) <= 1) {
        # Touching
        new_knot2 <- knot2
    } else {
        # If the head is ever two steps directly up, down, left, or right from the tail,
        # the tail must also move one step in that direction so it remains close enough:
        knot_diff <- knot1 - knot2
        new_knot2 <- case_when(
            # same row
            (knot1[2] == knot2[2]) && (knot_diff[1] == 2) ~ knot2 + c(1, 0), # move right
            (knot1[2] == knot2[2]) && (((knot1[1] - knot2[1]) == -2)) ~ knot2 + c(-1, 0), # move left
            # same col
            (knot1[1] == knot2[1]) && (((knot1[2] - knot2[2]) == 2)) ~ knot2 + c(0, 1), # move up
            (knot1[1] == knot2[1]) && (((knot1[2] - knot2[2]) == -2)) ~ knot2 + c(0, -1), # move down

            # diagonals
            ################### 1,1
            # ..H
            # ...
            # .T.
            (knot_diff[1] > 0 && knot_diff[2] > 0) ~ knot2 + c(1, 1),
            ################### -1,1
            # H
            #
            #  T
            (knot_diff[1] < 0 && knot_diff[2] > 0) ~ knot2 + c(-1, 1),
            ################### 1,-1
            # T
            #
            #  H
            (knot_diff[1] > 0 && knot_diff[2] < 0) ~ knot2 + c(1, -1),
            ################### -1,-1
            # .T
            # ..
            # H.
            (knot_diff[1] < 0 && knot_diff[2] < 0) ~ knot2 + c(-1, -1)
        )
    }
    return(new_knot2)
}

do_day_9_ii <- function(data, rope_knots = c("H", "1"), knot_of_interest = 2) {
    instructions <- data |>
        mutate(
            direction = str_split(x, "\\s", simplify = TRUE)[, 1],
            repeats = str_split(x, "\\s", simplify = TRUE)[, 2] |> as.numeric(),
        )
    rope <- list()
    for (knot in rope_knots) {
        rope[[knot]] <- c(0, 0)
    }

    knot_of_interest_places <- tibble(
        x = 0,
        y = 0
    )

    for (instruction in seq_len(nrow(instructions))) {
        direction <- instructions[instruction, ]$direction
        #
        repeats <- instructions[instruction, ]$repeats
        #
        for (rep in 1:repeats) {
            rope[[1]] <- move_head(direction, rope[[1]])
            for (knot_i in 2:length(rope)) {
                rope[[knot_i]] <- calc_new_knot_position(
                    rope[[knot_i - 1]],
                    rope[[knot_i]]
                )
            }
            knot_of_interest_places <- add_row(
                knot_of_interest_places,
                x = rope[[knot_of_interest]][1],
                y = rope[[knot_of_interest]][2]
            )
        }
    }
    return(nrow(distinct(knot_of_interest_places)))
}

#######################################################
###           do part 1
#######################################################
do_day_9_ii(sample_input_d9, rope_knots = c("H", "1"), knot_of_interest = 2)

do_day_9_ii(input_9, rope_knots = c("H", "1"), knot_of_interest = 2)

#######################################################
###           do part 2
#######################################################
do_day_9_ii(sample_input_d9, rope_knots = c("H", as.character(1:9)), knot_of_interest = 10)
do_day_9_ii(sample_input_2_d9, rope_knots = c("H", as.character(1:9)), knot_of_interest = 10)

do_day_9_ii(input_9, rope_knots = c("H", as.character(1:9)), knot_of_interest = 10)