library(dplyr)
library(tibble)
library(stringr)
library(tidyr)

#######################################################
###          get inputs
#######################################################
sample_input_14 <- tibble(
    x = str_split(
        "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9",
        "\n"
    ) |> unlist()
)
input_14 <- adventdrob::advent_input(14, 2022)

#######################################################
###          day 13 functions
#######################################################

#############################################
#  generate cave map/grid from instructions
#############################################
get_cave_map <- function(data, part = 1) {
    cave_map <- tibble(rock_n = integer(0), rocks = character(0))
    for (r in seq_len(nrow(data))) {
        cave_map <- cave_map |>
            add_row(
                rock_n = r,
                rocks = data[r, ][[1]] |>
                    str_split(" -> ") |>
                    unlist()
            )
    }
    cave_map <- cave_map |>
        extract(rocks, c("rock_x", "rock_y"), "(\\d*),(\\d*)") |>
        mutate(
            across(2:3, as.numeric)
        )

    for (r in seq_len(nrow(cave_map) - 1)) {
        # different instruction/rock formation
        if (cave_map[r, "rock_n"][[1]] != cave_map[r + 1, "rock_n"][[1]]) {
            next()
        }

        if (cave_map[r, "rock_x"][[1]] == cave_map[r + 1, "rock_x"][[1]]) {
            dir_y <- if_else((cave_map[r, "rock_y"][[1]] - cave_map[r + 1, "rock_y"][[1]]) < 0, 1, -1)
            for (y in seq(cave_map[r, "rock_y"][[1]], cave_map[r + 1, "rock_y"][[1]], by = dir_y)) {
                cave_map <- cave_map |>
                    add_row(
                        rock_n = cave_map[r, "rock_n"][[1]],
                        rock_x = cave_map[r, "rock_x"][[1]],
                        rock_y = y
                    )
            }
        } else if (cave_map[r, "rock_y"][[1]] == cave_map[r + 1, "rock_y"][[1]]) {
            dir_x <- if_else((cave_map[r, "rock_x"][[1]] - cave_map[r + 1, "rock_x"][[1]]) < 0, 1, -1)
            for (x in seq(cave_map[r, "rock_x"][[1]], cave_map[r + 1, "rock_x"][[1]], by = dir_x)) {
                cave_map <- cave_map |>
                    add_row(
                        rock_n = cave_map[r, "rock_n"][[1]],
                        rock_x = x,
                        rock_y = cave_map[r, "rock_y"][[1]]
                    )
            }
        }
    }
    cave_map <- cave_map |>
        rename(
            x = rock_x,
            y = rock_y
        ) |>
        mutate(block_type = "rock") |>
        select(-rock_n) |>
        distinct() |>
        arrange(x, y)

    if (part == 2) {
        max_depth <- max(cave_map$y) + 2
        # range of x should be max_depth either side of where sand starts,
        # but sim takes too long to want to check this..
        # -> go max_depth + 100 either side!
        min_x <- 400 - max_depth
        max_x <- 600 + max_depth
        cave_map <- cave_map |>
            bind_rows(
                tibble(
                    block_type = "rock",
                    x = min_x:max_x,
                    y = max_depth
                )
            )
    }
    return(cave_map)
}

#############################################
#  short move check function
#############################################
can_move <- function(map, position, move) {
    new_pos <- position + move
    ok <- sum((map$x == new_pos[1]) & (map$y == new_pos[2])) == 0
    return(ok)
}

#############################################
# do loop of one sand block falling until stops
# or falls into the abyss
#############################################
do_one_sand_loop <- function(map, sand_origin = c(500, 0)) {
    sand_block <- sand_origin
    max_depth <- max(map$y)
    sand_block_falling <- TRUE
    max_depth_reached <- FALSE
    moved_from_origin <- FALSE

    # loop for one sand block
    while (sand_block_falling) {
        if (sand_block[2] > max_depth) {
            # reached max depth
            # add to map
            map <- map |>
                add_row(
                    block_type = "sand",
                    x = sand_block[1],
                    y = sand_block[2]
                )
            sand_block_falling <- FALSE
            max_depth_reached <- TRUE
            next()
        }
        if (can_move(map, sand_block, c(0, 1))) {
            # fall down?
            sand_block <- sand_block + c(0, 1)
            moved_from_origin <- TRUE
        } else if (can_move(map, sand_block, c(-1, 1))) {
            # fall down left
            sand_block <- sand_block + c(-1, 1)
            moved_from_origin <- TRUE
        } else if (can_move(map, sand_block, c(1, 1))) {
            # fall down right
            sand_block <- sand_block + c(1, 1)
            moved_from_origin <- TRUE
        } else {
            # can't move ,add to map
            map <- map |>
                add_row(
                    block_type = "sand",
                    x = sand_block[1],
                    y = sand_block[2]
                )
            sand_block_falling <- FALSE
        }
    }
    return(list(map = map, max_depth_reached = max_depth_reached, moved_from_origin = moved_from_origin))
}

# #################################
#  Main sim loop with some fudging for part 1 vs 2
do_sand <- function(data, part = 1, verbose = FALSE) {
    cave_map <- get_cave_map(data, part)
    add_block <- TRUE
    block_n <- 0
    while (add_block) {
        block_n <- block_n + 1
        if (verbose) print(paste0("Block ", block_n))
        sand_loop_result <- do_one_sand_loop(cave_map)
        cave_map <- sand_loop_result$map
        if (sand_loop_result$max_depth_reached) add_block <- FALSE
        if (!sand_loop_result$moved_from_origin) add_block <- FALSE
    }
    if (part == 1) {
        cave_map <- cave_map |>
            head(-1)
    }
    return(cave_map)
}

###############################
#  Part 1
###############################
do_sand(sample_input_14) |> count(block_type)

# takes a while
do_sand(input_14) |> count(block_type) # 793

###############################
#  Part 2
###############################
do_sand(sample_input_14, part = 2) |> count(block_type) #

# this takes a really long time
# people probably did the whole day's challenge quicker than this sim takes!
# TODO learn julia
{
    tictoc::tic()
    cave_map_pt2 <- do_sand(input_14, part = 2) #
    tictoc::toc()
} # 374.219 sec elapsed!
cave_map_pt2 |> count(block_type)

###############################
#  Plots
###############################
library(ggplot2)
cave_map_pt2 |>
    mutate(y = min(y) - y) |>
    ggplot() +
        geom_raster(aes(x, y, fill = block_type)) +
        scale_fill_viridis_d() +
        theme_void() +
        theme(legend.position = "none")
ggsave(
    here::here("img", "day_14.jpg")
)
