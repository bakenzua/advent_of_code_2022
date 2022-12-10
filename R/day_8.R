library(dplyr)
library(stringr)
library(ggplot2)
library(rayshader) # optional

#######################################################
###           Get inputs
#######################################################
sample_input <- tibble(
  x = str_split(
    "30373
25512
65332
33549
35390",
    "\n"
  ) |> unlist()
)

input_8 <- adventdrob::advent_input(8, 2022)

#######################################################
###           do_day_8 function
#######################################################

do_day_8 <- function(data, return_dataframe = FALSE) {
  grid <- data |>
    adventdrob::grid_tidy(x)

  total_rows <- nrow(data)
  total_cols <- nchar(data$x[1])

  do_vertical_visibility <- function(data, c) {
    data |>
      filter(col == c) |>
      mutate(
        vis_top = value > lag(cummax(value))
      ) |>
      arrange(desc(row)) |>
      mutate(
        vis_bottom = value > lag(cummax(value))
      )
  }

  do_horiz_visibility <- function(data, c) {
    data |>
      filter(row == c) |>
      mutate(
        viz_left = value > lag(cummax(value))
      ) |>
      arrange(desc(col)) |>
      mutate(
        viz_right = value > lag(cummax(value))
      )
  }

  vgrid <- do_vertical_visibility(grid, 1)
  for (i in 2:total_cols) {
    vgrid <- bind_rows(
      vgrid, do_vertical_visibility(grid, i)
    )
  }
  hgrid <- do_horiz_visibility(grid, 1)
  for (i in 2:total_rows) {
    hgrid <- bind_rows(
      hgrid, do_horiz_visibility(grid, i)
    )
  }
  grid <- left_join(hgrid, vgrid, by = c("row", "value", "col")) |>
    mutate(
      vis_top = if_else(row == 1, TRUE, vis_top),
      vis_bottom = if_else(row == total_rows, TRUE, vis_bottom),
      viz_left = if_else(col == 1, TRUE, viz_left),
      viz_right = if_else(col == total_cols, TRUE, viz_right),
      visible = vis_top | vis_bottom | viz_left | viz_right
    )
  if (return_dataframe) {
    return(grid)
  } else {
    return(sum(grid$visible))
  }
}

#######################################################
###           do_day_8_part2 function
#######################################################

do_day_8_part2 <- function(data, return_dataframe = FALSE) {
  tree_grid <- data |>
    adventdrob::grid_tidy(x) |>
    mutate(
      vis_up = NA_real_,
      vis_down = NA_real_,
      vis_left = NA_real_,
      vis_right = NA_real_
    )

  total_rows <- nrow(data)
  total_cols <- nchar(data$x[1])

  vis_n <- function(v) {
    if (length(v) == 1) {
      return(0)
    } else {
      for (i in 2:length(v)) {
        if (v[i] >= v[1]) {
          return(i - 1)
        }
      }
    }
    return(length(v) - 1)
  }

  for (row_i in 1:total_rows) {
    tree_row <- tree_grid |>
      filter(row == row_i) |>
      pull(value)

    for (tree_i in seq_along(tree_row)) {
      tree_grid <- tree_grid |>
        mutate(
          vis_right = if_else(
            (row == row_i) & (col == tree_i),
            vis_n(tree_row[tree_i:length(tree_row)]),
            vis_right
          ),
          vis_left = if_else(
            (row == row_i) & (col == tree_i),
            vis_n(tree_row[tree_i:1]),
            vis_left
          )
        )
    }
  }

  for (col_i in 1:total_cols) {
    tree_col <- tree_grid |>
      filter(col == col_i) |>
      pull(value)

    for (tree_i in seq_along(tree_col)) {
      tree_grid <- tree_grid |>
        mutate(
          vis_down = if_else(
            (row == tree_i) & (col == col_i),
            vis_n(tree_col[tree_i:length(tree_col)]),
            vis_down
          ),
          vis_up = if_else(
            (row == tree_i) & (col == col_i),
            vis_n(tree_col[tree_i:1]),
            vis_up
          )
        )
    }
  }
  tree_grid <- tree_grid |>
    mutate(
      scenic_score = vis_up * vis_down * vis_right * vis_left
    )

  if (return_dataframe) {
    return(tree_grid)
  } else {
    return(max(tree_grid$scenic_score))
  }
}

#######################################################
###           do part 1
#######################################################
do_day_8(sample_input)

do_day_8(input_8)

# plots
plot_visible <- do_day_8(input_8, return_dataframe = TRUE) |>
  mutate(
    value = if_else(visible, value + 3, value)
  ) |>
  ggplot(aes(row, col)) +
  geom_raster(aes(fill = value)) +
  scale_fill_viridis_c(option = "A") +
  theme_bw()
plot_visible

# plot_gg(plot_visible, multicore=TRUE,height=5,width=6,scale=500)
# render_snapshot()

#######################################################
###           do part 2
#######################################################

do_day_8_part2(sample_input)

do_day_8_part2(input_8)

df <-  input_8 |> 
  adventdrob::grid_tidy(x)
plot2 <- df |>
  ggplot(aes(row, col, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "A") +
  theme_bw())
plot2

plot_gg(plot2, multicore=TRUE,height=5,width=6,scale=500)
render_snapshot(