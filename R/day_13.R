library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(purrr)

#######################################################
###          get inputs
#######################################################
sample_input_13  <- tibble(
    x = str_split(
        "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]",
"\n"
    )  |> unlist()
)

input_13  <- adventdrob::advent_input(13, 2022)

#######################################################
###          day 13 functions
#######################################################
# chop the inputs up into pair of lines and spread to
# left and right columns
part_1_wrangle_inputs <- function(data) {
    df <- data |>
        filter(x != "")
    df$id <- rep(seq_len((nrow(df) / 2)), each = 2)
    df$name <- rep(c("left", "right"), times = (nrow(df) / 2))
    return(df)
}
# monstrosity of a comparator function implementing the logic
# comparing if packets are in the right order
is_correct_order <- function(left, right, verbose = FALSE) {
  # use JSON parser to parse into values or lists
  # is this cheating?
  lt <- jsonlite::fromJSON(left)
  rt <- jsonlite::fromJSON(right)

  if (verbose) print("-")
  if (verbose) print(paste0("left: ", lt))
  if (verbose) print(paste0("right: ", rt))

  llen <- length(lt)
  rlen <- length(rt)

  if ((llen == 0) && (rlen > 0)) {
    return(TRUE)
  } else if ((llen > 0) && (rlen == 0)) {
    return(FALSE)
  }
  for (i in 1:min(llen, rlen)) {
    left_i <- lt[i]
    right_i <- rt[i]

    # both integers
    if ((class(left_i) == "integer") && (class(right_i) == "integer")) {
      if (verbose) print(paste0("Both integers: ", left_i, ", ", right_i))
      if (left_i < right_i) {
        if (verbose) print("COrrect Order!")
        return(TRUE)
      } else if (left_i > right_i) {
        if (verbose) print("IncOrrect Order!")
        return(FALSE)
      }
      next()
    }

    # both lists
    if ((class(left_i) == "list") && (class(right_i) == "list")) {
      if ((length(left_i[[1]]) == 0) && length(right_i[[1]]) == 0) {
        if (verbose) print("both null")
        next()
      } else {
        if (verbose) print(paste0("both lists send to sub: ", left_i, "||", right_i))
        sub_return <- is_correct_order(jsonlite::toJSON(left_i[[1]]), jsonlite::toJSON(right_i[[1]]))
        if (verbose) print("return from sub")
        if (length(sub_return) != 0) {
          return(sub_return)
        } else {
          next()
        }
      }
    }


    # # only one is an int
    if (class(left_i) == "integer") {
      if (verbose) print(paste0("Only left is an integer: ", left_i, ", ", right_i))
      left_i <- list(left_i)
    } else if (class(right_i) == "integer") {
      if (verbose) print(paste0("Only right is an integer: ", left_i, ", ", right_i))
      right_i <- list(right_i)
    } else {
      stop("SOmething h0rrif!c happened and we d!dn't catch l!sts")
    }

    if (verbose) print(paste0("both lists, one now converted to list", left_i, right_i))
    sub_return <- is_correct_order(jsonlite::toJSON(left_i[[1]]), jsonlite::toJSON(right_i[[1]]))
    if (length(sub_return) != 0) {
      return(sub_return)
    } else {
      (
        next()
      )
    }
  }

  # if get this far compare lengths
  if (llen < rlen) {
    if (verbose) print(paste0("Compare lengths: ", llen, " ", rlen, "COrrect Order!"))
    return(TRUE)
  } else if (llen > rlen) {
    if (verbose) print(paste0("Compare lengths: ", llen, " ", rlen, "InCOrrect Order!"))
    return(FALSE)
  }
}
# lost an hour trying to implement custom class 
# with comparison operator of 'is_correct_order()'!
# bubble sort for the win!
# (Could have just counted the n of messages before given divider messages!)
bubble_sort <- function(x) {
  swap_performed <- TRUE
  # Repeat until no more swaps
  while (swap_performed) {
    swap_performed <- FALSE
    # Check if swap
    for (i in 1:(length(x) - 1)) {
      if (!is_correct_order(x[i], x[i + 1])) {
        # Swap elem
        tmp <- x[i]
        x[i] <- x[i + 1]
        x[i + 1] <- tmp
        # swapped
        swap_performed <- TRUE
      }
    }
  }
  # Output: the vector sorted in increasing order
  return(x)
}
###############################
#  Tests
###############################
tdf <- sample_input_13 |>
    part_1_wrangle_inputs()  |>
    pivot_wider(id, values_from = x, names_from = name) |>
    mutate(
        correct_order = map2_lgl(left, right, is_correct_order)
    )
sum(tdf$correct_order == c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)) == nrow(tdf)

###############################
#  Part 1
###############################
sample_input_13 |> 
    part_1_wrangle_inputs()  |>
    # vestigal first attempt at hacking the input to
    # actual data objects
    # Note: Have seen a 'wild' version using str2lang since puzzle completion!
    # mutate(
    #     x = str_replace_all(x, "\\[", "list("),
    #     x = str_replace_all(x, "]", ")"),
    #     x = list(eval(parse(text=x)))
    # ) |>
    pivot_wider(id, values_from = x, names_from = name) |>
    mutate(
        correct_order = map2_lgl(left, right, is_correct_order)
    ) |>
    filter(correct_order) |>
    summarise(result = sum(id))

input_13  |>
    part_1_wrangle_inputs()  |>
    pivot_wider(id, values_from = x, names_from = name)  |>
    mutate(
        correct_order = map2_lgl(left, right, is_correct_order)
    ) |>
    filter(correct_order) |>
    summarise(result = sum(id))

###############################
#  Part 2
###############################
s <- c(filter(sample_input_13, x != "")$x, "[[2]]", "[[6]]")

s <- bubble_sort(s)
which(s == "[[2]]") * which(s == "[[6]]")

r <- c(filter(input_13, x != "")$x, "[[2]]", "[[6]]")
r <- bubble_sort(r)
# bubble sort for how long!
which(r == "[[2]]") * which(r == "[[6]]")
# or this post puzzle method, simply add up all those smaller/before divider packets
(sum(
    sapply(filter(input_13, x != "")$x, function(x) is_correct_order(x, '[[2]]'))
) + 1) *
(sum(
    sapply(filter(input_13, x != "")$x, function(x) is_correct_order(x, '[[6]]'))
) + 2) # + 2 because need to account for previous divider as well
