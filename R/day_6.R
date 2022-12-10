library(dplyr)
library(tibble)
library(stringr)

#######################################################
###           Get inputs
#######################################################
sample_input_d6 <- tibble(
    x = c(
        "bvwbjplbgvbhsrlpgdmjqwftvncz",
        "nppdvjthqldpwncqszvftbrmjlhg",
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    ),
    n = c(5, 6, 10, 11)
)
sample_input_2_d6 <- tibble(
    x = c(
        "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
        "bvwbjplbgvbhsrlpgdmjqwftvncz",
        "nppdvjthqldpwncqszvftbrmjlhg",
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    ),
    n = c(19, 23, 23, 29, 26)
)

input_6 <- adventdrob::advent_input(6, 2022)


#######################################################
###           Day 6 function
#######################################################

detect_start_marker <- function(str, start_n = 4) {
    # diy rolling window
    for (i in seq(start_n, nchar(str))) {
        n <- str_split(
            # spent too long time debugging accidental -1 vs +1 here!
            str_sub(str, i - start_n + 1, i),
            ""
        )[[1]] |>
            n_distinct()
        # hacky break
        if (n == start_n) break()
    }
    return(i)
}

#######################################################
###           Detect start of packets
#######################################################

sample_input_d6 |>
    mutate(
        start = map_int(x, detect_start_marker)
    )

# part 1
detect_start_marker(input_6, start_n = 4)

#######################################################
###           Detect start of messages
#######################################################
sample_input_2_d6 |>
    mutate(
        start = map_int(x, detect_start_marker, start_n = 14)
    )

detect_start_marker(input_6, start_n = 14)