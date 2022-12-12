#######################################################
###           Load packages
#######################################################
library(tidyverse)
library(data.tree)

#######################################################
###           Get inputs
#######################################################
sample_input <- tibble(
    x = strsplit(
        "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k",
        "\n"
    ) |> unlist()
)

input_7 <- adventdrob::advent_input(7, 2022)


#######################################################
###           Day 7 functions
#######################################################

do_day_7 <- function(data, part_1 = TRUE) {
    # mangle the input data a little to help with building the tree
    data <- data |>
        filter(!str_detect(x, "^\\$ ls")) |>
        mutate(
            line_type = case_when(
                str_detect(x, "^\\$ cd /$") ~ "cd_root",
                str_detect(x, "^\\$ cd \\.\\.") ~ "cd_up",
                str_detect(x, "^\\$ cd [\\w]*") ~ "cd_down",
                str_detect(x, "^\\$ ls") ~ "ls",
                str_detect(x, "^dir ") ~ "dir",
                str_detect(x, "^[:digit:]* ") ~ "file"
            ),
            file_name = if_else(
                line_type == "file",
                str_split(x, "\\s", simplify = TRUE)[, 2],
                NA_character_
            ),
            file_size = if_else(
                line_type == "file",
                suppressWarnings(as.numeric(str_split(x, "\\s", simplify = TRUE)[, 1])),
                NA_real_
            ),
            dir_name = case_when(
                line_type == "cd_root" ~ "/",
                line_type == "cd_down" ~ str_extract(x, "\\w*$"),
            )
        )

    # make a data.tree root node
    disk <- Node$new("/")
    # This is the nice bit as it dynamically adds up sizes of children in the tree on each traversal
    disk$size <- function(self) sum(sapply(self$children, function(child) GetAttribute(child, "size")), na.rm = TRUE)
    disk$dir <- TRUE
    current_node <- disk
    # build the tree of folders and files
    for (i in 1:nrow(data)) {
        if (data[i, ]$line_type == "cd_root") {
            current_node <- current_node$root
        }
        if (data[i, ]$line_type == "cd_down") {
            current_node <- current_node$AddChild(paste0("/", data[i, ]$dir_name))
            current_node$dir <- TRUE
            current_node$size <- function(self) sum(sapply(self$children, function(child) GetAttribute(child, "size")), na.rm = TRUE)
        }
        if (data[i, ]$line_type == "cd_up") {
            current_node <- current_node$parent
        }
        if (data[i, ]$line_type == "file") {
            file <- current_node$AddChild(data[i, ]$file_name)
            file$size <- data[i, ]$file_size
            file$dir <- FALSE
        }
    }
    # back to data frame of dirs only
    disk_df <- disk_as_df <- ToDataFrameTree(disk, "dir", "size")

    if (part_1) {
        answer <- disk_df |>
            filter(
                dir,
                size <= 100000
            ) |>
            pull(size) |>
            sum()
    } else {
        root_size <- disk_df |>
            head(1) |>
            pull(size)

        space_required <- 30000000 - (70000000 - root_size) # could simplify but easier to read

        answer <- disk_df |>
            filter(
                dir,
                size >= space_required
            ) |>
            arrange(size) |>
            head(1) |>
            pull(size)
    }

    return(answer)
}

# TODO Do this as a tidygraph tree instead of data.tree

#######################################################
###           Part 1
#######################################################
do_day_7(sample_input)

do_day_7(input_7)

#######################################################
###           Part 2
#######################################################

do_day_7(sample_input, part_1 = FALSE)

do_day_7(input_7, part_1 = FALSE)
