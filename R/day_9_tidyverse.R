library(dplyr)
library(stringr)
library(tibble)

#######################################################
###           Get inputs
#######################################################
sample_input <- tibble(
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
sample_input_2 <- tibble(
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

#####################################################
##  calculate knot position
#####################################################
calc_new_knot_position <- function(knot1, knot2) {
    new_knot2_position <- knot2 |>
        mutate(time = time + 1)

    if ((abs(knot2$x - knot1$x) <= 1) && (abs(knot2$y - knot1$y) <= 1)) {
        # Touching
    } else {
        # If the head is ever two steps directly up, down, left, or right from the tail,
        # the tail must also move one step in that direction so it remains close enough:
        #
        # same row
        if (knot2$y == knot1$y) {
            if (((knot1$x - knot2$x) == 2)) {
                new_knot2_position$x <- knot2$x + 1
            } else {
                new_knot2_position$x <- knot2$x - 1
            }
        }
        # same col
        if (knot2$x == knot1$x) {
            if ((knot1$y - knot2$y) == 2) {
                new_knot2_position$y <- knot2$y + 1
            } else {
                new_knot2_position$y <- knot2$y - 1
            }
        }

        # diagonals
        ################### 1,1
        # ..H
        # ...
        # .T.
        if ((knot2$x < knot1$x) && (knot2$y < knot1$y)) {
            new_knot2_position$x <- knot2$x + 1
            new_knot2_position$y <- knot2$y + 1
        }

        ################### -1,1
        # H
        #
        #  T
        if ((knot2$x > knot1$x) && (knot2$y < knot1$y)) {
            new_knot2_position$x <- knot2$x - 1
            new_knot2_position$y <- knot2$y + 1
        }
        ################### 1,-1
        # T
        #
        #  H
        if ((knot2$x < knot1$x) && (knot2$y > knot1$y)) {
            new_knot2_position$x <- knot2$x + 1
            new_knot2_position$y <- knot2$y - 1
        }
        ################### -1,-1
        # .T
        # ..
        # H.
        if ((knot2$x > knot1$x) && (knot2$y > knot1$y)) {
            new_knot2_position$x <- knot2$x - 1
            new_knot2_position$y <- knot2$y - 1
        }
    }
    return(new_knot2_position)
}

#####################################################
##  do day 9
#####################################################
do_day_9 <- function(data, rope = c("H", "1"), verbose = FALSE, return_positions = FALSE) {
    # parse instructions
    instructions <- data |>
        mutate(
            direction = str_split(x, "\\s", simplify = TRUE)[, 1],
            repeats = str_split(x, "\\s", simplify = TRUE)[, 2] |> as.numeric(),
        )
    # initialise rope
    positions <- tibble(
        knot = rope
    ) |>
        mutate(
            x = 0,
            y = 0,
            time = 0
        )
    #
    for (instruction in seq_len(nrow(instructions))) {
        direction <- instructions[instruction, ]$direction
        #
        repeats <- instructions[instruction, ]$repeats
        #
        if (verbose) print(paste0("Instruction", instruction, ": ", direction, " ", repeats))
        for (rep in 1:repeats) {
            if (verbose) print(paste0("Repeat: ", rep))
            current_time <- max(positions$time) + 1
            current_head_pos <- positions |>
                filter(
                    knot == "H",
                    time == current_time - 1
                )
            # if(nrow(current_head_pos) > 1) stop('More than 1 head row found')
            new_head_row <- tibble_row(
                knot = "H",
                x = case_when(
                    direction == "L" ~ current_head_pos$x - 1,
                    direction == "R" ~ current_head_pos$x + 1,
                    TRUE ~ current_head_pos$x
                ),
                y = case_when(
                    direction == "U" ~ current_head_pos$y + 1,
                    direction == "D" ~ current_head_pos$y - 1,
                    TRUE ~ current_head_pos$y
                ),
                time = current_time
            )
            positions <- positions |> add_row(new_head_row)

            for (knot_id in 2:length(rope)) {
                new_knot_row <- calc_new_knot_position(
                    new_head_row,
                    positions |>
                        filter(
                            knot == rope[knot_id],
                            time == current_time - 1
                        )
                )
                positions <- positions |> add_row(new_knot_row)
                new_head_row <- new_knot_row
            }
        }
    }
    unique_tailxy <- positions |>
        filter(knot == rope[length(rope)]) |>
        select(x, y) |>
        distinct() |>
        nrow()
    if (return_positions) {
        return(positions)
    } else {
        return(unique_tailxy)
    }
}

#######################################################
###           do part 1
#######################################################
do_day_9(sample_input) |> print() # 13 # 0.22 secs
do_day_9(input_9) |> print() # 6011

#######################################################
###           do part 2
#######################################################
do_day_9(sample_input, rope = c("H", 1:9)) |> print() # 1 # 1.1 secs!

do_day_9(sample_input_2, rope = c("H", 1:9)) |> print() # 36 # 4.22sec

# super slow!
do_day_9(input_9, rope = c("H", 1:9))
