library(tibble)
library(stringr)
library(tidyr)

#######################################################
###           Get inputs
#######################################################
sample_input <- tibble(
    x = str_split(
        "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1",
        "\n"
    ) |> unlist()
)

input_11 <- adventdrob::advent_input(11, 2022)


#######################################################
###           Day 11 functions
#######################################################
parse_instructions <- function(data) {
    data |>
        mutate(
            monkey = if_else(
                str_detect(x, "^Monkey"),
                str_extract(x, "Monkey \\d"),
                NA_character_
            ),
            starting_items = if_else(
                str_detect(x, "Starting items: "),
                str_sub(x, "19"),
                NA_character_
            ),
            # starting_items = str_split(starting_items, ", "),
            operation_operator = if_else(
                str_detect(x, "Operation"),
                str_sub(x, 24, 24), NA_character_
            ),
            operation_operand = if_else(
                str_detect(x, "Operation"),
                str_sub(x, 26), NA_character_
            ),
            divisible_by = if_else(
                str_detect(x, "Test"),
                str_extract(x, "\\d*$") |> as.numeric(),
                NA_real_
            ),
            if_true = if_else(
                str_detect(x, "If true"),
                str_extract(x, "\\d*$") |> as.numeric(),
                NA_real_
            ),
            if_false = if_else(
                str_detect(x, "If false"),
                str_extract(x, "\\d*$") |> as.numeric(),
                NA_real_
            )
        ) |>
        fill(monkey) |>
        group_by(monkey) |>
        fill(2:8, .direction = "updown") |>
        filter(x != "") |>
        select(-x) |>
        distinct() |>
        mutate(
            starting_items = str_split(starting_items, ", ")
        )
}

do_monkey_instructions <- function(data, rounds = 20, div_worry_by = 3) {
    # get parsed instructions
    instructions <- parse_instructions(data)
    #  set up item and examination lists
    items <- list()
    examinations <- list()
    for (monkey in seq_len(nrow(instructions))) {
        items[[monkey]] <- instructions$starting_items[monkey] |>
            unlist() |>
            as.numeric()
        examinations[[monkey]] <- 0
    }

    # get product of div test numbers,
    # to limit the 'item worry' but still allow each monkeys modulus tests to work
    common_multiple <- prod(instructions$divisible_by)
    # same as least_common_multiple as all instructions$divisible_by are prime
    # least_common_multiple <- numbers::mLCM(instructions$divisible_by)

    round_history <- list()

    for (round in 1:rounds) {
        # provide feedback on round, reassuring in part 2
        if ((round %% 100) == 0) {
            cat(paste0(round, " of ", rounds, "\r"))
            flush.console()
        }

        for (monkey_id in seq_len(nrow(instructions))) {
            for (monkey_item in seq_along(items[[monkey_id]])) {
                # get item
                item <- items[[monkey_id]][monkey_item] |> as.numeric()

                examinations[[monkey_id]] <- examinations[[monkey_id]] + 1

                # calc new worry level
                operation_operator <- instructions[monkey_id, "operation_operator"]
                operation_operand <- instructions[monkey_id, "operation_operand"]

                if (operation_operand == "old") {
                    operation_operand <- item
                } else {
                    operation_operand <- as.numeric(operation_operand)
                }

                if (operation_operator == "*") {
                    # change 3 to div_worry_by for part 2
                    # stop worrying and learn to love the monkeys
                    item <- (item * operation_operand) %/% div_worry_by
                } else if (operation_operator == "+") {
                    item <- (item + operation_operand) %/% div_worry_by
                }

                # modulo by common_multiple to stop 'item worry' getting very very very big*
                # this took a little while to work out what was going wrong, modulus %% in R fails with big numbers
                item <- item %% common_multiple

                # test div by and select destination monkey
                destination_monkey <- if_else(
                    (item %% instructions[monkey_id, "divisible_by"] == 0),
                    # account for indexing 0
                    destination_monkey <- instructions[[monkey_id, "if_true"]] + 1,
                    destination_monkey <- instructions[[monkey_id, "if_false"]] + 1
                )

                # throw item to another monkey
                items[[destination_monkey]] <- c(
                        items[[destination_monkey]],
                        item
                    )
            }
            # empty monkey's item list as has thrown all items
            # spent way too long on this bit, as setting to c() and deleting this monkey item entry
            items[[monkey_id]] <- list(c())
        }
        items <- lapply(items, unlist)
    }
    monkey_business <- examinations |>
        unlist() |>
        sort(decreasing = TRUE) |>
        head(2) |>
        prod()

    results <-  list("examinations" = examinations, "items" = items, "monkey_business" = monkey_business)
    
    return(results)
}


#######################################################
###           Do problems
#######################################################
# part 1
do_monkey_instructions(sample_input, rounds = 20)$monkey_business == 10605
do_monkey_instructions(input_11, rounds = 20)$monkey_business #110264


# part 2
do_monkey_instructions(sample_input, rounds = 10000, div_worry_by = 1)$monkey_business # 2713310158
do_monkey_instructions(input_11, rounds = 10000, div_worry_by = 1)$monkey_business