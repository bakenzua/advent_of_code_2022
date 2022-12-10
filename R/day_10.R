library(tidyverse)

#######################################################
###           Get inputs
#######################################################

sample_input <- tibble(
    x = str_split(
        "noop
addx 3
addx -5",
        "\n"
    ) |> unlist()
)
sample_input2 <- tibble(
    x = str_split(
        "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop",
        "\n"
    ) |> unlist()
)

input_10 <- adventdrob::advent_input(10, 2022)


###################################################################

get_register_states <- function(data) {
    instructions <- data |>
        mutate(
            instruction = str_split(x, "\\s", simplify = TRUE)[, 1],
            value = str_split(x, "\\s", simplify = TRUE)[, 2] |> as.numeric(),
        )
    instructions_n <- nrow(instructions)

    # set up register list and other machine variables
    x_history <- list()
    run <- TRUE
    x <- 1
    instruction_pointer <- 1
    addx_value <- NA_real_
    addx_counter <- 0
    cycle <- 1

    # run machine
    while (run) {
        # get instructions
        cycle_instruction <- instructions[instruction_pointer, ]$instruction
        cycle_value <- instructions[instruction_pointer, ]$value

        # update register history
        x_history[cycle] <- x

        if (cycle_instruction == "noop") {
            instruction_pointer <- instruction_pointer + 1
        } else {
            # addx
            if (addx_counter == 1) {
                x <- addx_value + x
                addx_counter <- 0
                instruction_pointer <- instruction_pointer + 1
            } else {
                addx_counter <- addx_counter + 1
                addx_value <- cycle_value
            }
        }

        # check instructions left
        run <- instruction_pointer <= instructions_n
        # increment cycle
        cycle <- cycle + 1
    }
    df <- tibble(
        x = x_history |> unlist()
    ) |>
        mutate(
            cycle = row_number(),
            xx = x * cycle, # part one

            # part 2 calcs
            # correcting for off by one issues and pixel vs cycle transforms
            row = ((cycle - 1) %/% 40) + 1,
            col = ((cycle - 1) %% 40) + 1,
            sprite_start = x,
            sprite_end = x + 2,
            pixel = if_else(
                (col >= sprite_start) & (col <= sprite_end),
                1,
                0
            ),
            pixel_content = if_else(pixel == 1, "#", ".")
        )
    return(df)
}

part_2_line_printer <- function(df) {
    data <- get_register_states(df)

    pixels <- data$pixel_content
    for (i in seq(0, 200, by = 40)) {
        print(paste0(pixels[(i + 1):(i + 40)], collapse = ""))
    }
}

# part 1
df <- get_register_states(input_10) |>
    filter(cycle %in% seq(20, 220, by = 40))
sum(df$xx) # 16060

# part 2
part_2_line_printer(sample_input2)

part_2_line_printer(input_10)

get_register_states(input_10) |>
    mutate(row = max(row) - row) |>
    ggplot(aes(row, col, fill = pixel)) +
    geom_raster() +
    coord_flip()
