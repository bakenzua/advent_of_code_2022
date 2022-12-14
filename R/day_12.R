library(tidyverse)
library(tidygraph)
# library(ggraph)

#######################################################
###          get inputs
#######################################################
sample_input_12 <- tibble(
  x = str_split(
    "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi",
    "\n"
  ) |> unlist()
) |>
  adventdrob::grid_tidy(x)

input_12 <- adventdrob::advent_input(12, 2022) |>
  adventdrob::grid_tidy(x)

#######################################################
###          day 12 function
#######################################################
make_heightmap_graph <- function(grid_data, dest_value = 26L) {
  # take in grid data as per adventdrob::grid_tidy() 
  # one grid square per row
  data <- grid_data |>
    mutate(
      # calculate height of each grid square
      height = match(value, letters),
      height = if_else(value == "S", 1L, height),
      height = if_else(value == "E", dest_value, height)
    )

  # calculate dimensions of lattice graph
  dimensions <- c(max(data$col), max(data$row))

  # create a tidy_graph with each grid square as a vertex
  # with directed edges in both directions between each,
  # no wrapping at edges of grid
  my_graph_g <- create_lattice(
    dimensions,
    directed = TRUE,
    mutual = TRUE,
    circular = FALSE
  ) |>
    mutate(data) |>
    activate("edges") |>
    mutate(
      from_elevation = .N()$height[from],
      to_elevation = .N()$height[to],
      from_label = .N()$value[from],
      to_label = .N()$value[to]
    ) |>
    filter(
      to_elevation <= (from_elevation + 1)
    ) |>
    activate("nodes") |>
    mutate(
      # at this point tried and failed to implement DIY Dijkstra shortest path
      # https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
      # use built in tidygraph function!
      distance_to_E = node_distance_to(
        # bit of a hack to find E
        # reliant on only one E
        which(.N()$value == "E"),
        mode = "out"
      )
    )
  return(as_tibble(my_graph_g, "nodes"))
}

#####################################################
###          part 1
#######################################################
sample_input_12_g <- make_heightmap_graph(sample_input_12)
sample_input_12_g |>
  filter(value == "S")

input_12_g <- make_heightmap_graph(input_12)

input_12_g |>
  filter(value == "S") # 528

#######################################################
###          part 2
#######################################################
input_12_g |>
  filter(
    value == "a",
    # lots of starting points from which a path to E does
    # not exist with inifnite length
    !(is.infinite(distance_to_E))
  ) |>
  arrange(distance_to_E) |>
  slice(1) # 522

#######################################################
###          plot with sketchy colour scheme
#######################################################
input_12_g |>
  mutate(
    distance_to_E = if_else(
      is.infinite(distance_to_E),
      NA_real_,
      distance_to_E
    ),
    row = min(row) - row
  ) |>
  filter(
    !(is.infinite(distance_to_E))
  ) |>
  ggplot(aes(row, col, fill = distance_to_E)) +
  geom_raster() +
  coord_flip() +
  scale_fill_viridis_c(
    na.value = "black",
    direction = -1,
    option = "A",
    begin = 0.4
  ) +
  theme_void() +
  theme(legend.position = "none")
