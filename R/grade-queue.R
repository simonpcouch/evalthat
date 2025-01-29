#' Grade an evaluation data frame
#'
#'
#' If you set the `judges` argument to a non-NULL value in [evaluate()],
#' this function will be evoked automatically.
#'
#' @param judges A [judges()] object or `NULL` to grade the evals yourself.
#' @param type One of `"pairwise"` or `"score"`. See details for more.
#'
#'
#' @examples
#' library(ellmer)
#'
#' ggplot2 <- evaluate(
#'   "tests/evalthat/test-ggplot2-graded.R",
#'     across = tibble(chat = c(
#'         chat_openai(model = "gpt-4o-mini", echo = FALSE),
#'         chat_claude(model = "claude-3-5-sonnet-latest", echo = FALSE),
#'         chat_ollama(model = "qwen2.5-coder:14b", echo = FALSE))
#'       ),
#'       repeats = 2
#'     )
#
# load("inst/sandbox/ggplot2.rda")
#'
#' ggplot2
#'
# ggplot2_graded <- grade_queue(
#   ggplot2,
#   judges = judges(
#     "gpt-4o-mini" = chat_openai(model = "gpt-4o-mini", echo = FALSE),
#     "claude-3-5-sonnet-latest" = chat_claude(model = "claude-3-5-sonnet-latest", echo = FALSE),
#     "qwen2.5-coder:14b" = chat_ollama(model = "qwen2.5-coder:14b", echo = FALSE))
#   )
# )
#'
#' ggplot2_graded
# @details
# TODO: describe pairwise vs. scoring
#
grade_queue <- function(x, judges, type = c("pairwise", "score")) {
  check_inherits(x, "evals_df")
  type <- arg_match(type)

  if (!is.null(judges)) {
    check_inherits(judges, "judges")
  } else {
    cli::cli_abort("Human grading not supported quite yet.")
  }

  rlang::eval_bare(
    rlang::call2(paste0("grade_queue_", type), x = x, judges = judges)
  )

}

# pairwise ---------------------------------------------------------------------
grade_queue_pairwise <- function(x, judges) {
  # TODO: check that there's more than one model responding

  queue <- unnest_queue(x)

  graded_pairs <- tibble()
  queue_by_input <- queue %>% dplyr::group_split(input)
  for (queue_input in queue_by_input) {
    # TODO: sample locations of pairings of distinct values
    unique_configuration_locs <-
      vctrs::vec_group_id(
        queue_input[seq_len(which(colnames(queue_input) == "input") - 1)]
      )

    pairings <- generate_pairs(unique_configuration_locs)

    input_graded_pairs <-
      purrr::pmap(pairings, function(row1, row2) {
        tibble(
          # TODO: add the metadata on the Chats here
          judge = list(judges),
          input = queue_input$input[1],
          target = queue_input$target[1],
          response_a = queue_input$output[row1],
          response_b = queue_input$output[row2],
          # TODO: this needs to be more general to any kind of varying out
          config_a = list(queue_input$chat[row1]),
          config_b = list(queue_input$chat[row2])
        )
      }) %>%
      purrr::list_rbind() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(result = list(grade_pair_safely(
        judge = judge,
        input = input,
        target = target,
        response_a = response_a,
        response_b = response_b
      )))

    graded_pairs <- dplyr::bind_rows(graded_pairs, input_graded_pairs)
  }

  graded_pairs
}

# for group ids e.g. 1 1 1 1 1 2 2 2, make a minimal set of
# pairings s.t. every entry is paired with another from a different group
generate_pairs <- function(unique_ids) {
  # get indices for each unique group
  groups <- split(seq_along(unique_ids), unique_ids)

  # initialize empty pairs dataframe
  pairs <- data.frame(row1 = integer(), row2 = integer())

  # for each group, pair with rows from next group (cycling back to first)
  for (i in seq_along(groups)) {
    current_group <- groups[[i]]
    next_group <- groups[[(i %% length(groups)) + 1]]

    # create pairs, cycling through next_group as needed
    new_pairs <- data.frame(
      row1 = current_group,
      row2 = next_group[((seq_along(current_group) - 1) %% length(next_group)) + 1]
    )

    pairs <- rbind(pairs, new_pairs)
  }

  # remove pairs that are just switched around versions of another
  pairs <- transform(pairs, row1 = pmin(row1, row2), row2 = pmax(row1, row2))
  pairs <- unique(pairs)

  return(pairs)
}

grade_pair <- function(judges, input, target, response_a, response_b) {
  prompt_original <-
    grade_pair_prompt(
      input = input,
      target = target,
      response_a = response_a,
      response_b = response_b
    )

  prompt_switched <-
    grade_pair_prompt(
      input = input,
      target = target,
      response_a = response_b,
      response_b = response_a
    )

  res_original <- lapply(judges, grade_pair_impl, prompt_original)
  res_switched <- lapply(judges, grade_pair_impl, prompt_switched)

  merge_graded_pairings(res_original, res_switched)
}

# merge a pair of grade_pair_impl() outputs by, for each judge, voting to determine
# the position-consistent winner.
merge_graded_pairings <- function(original, switched) {
  res <- list()
  judges <- names(original)

  for (judge in judges) {
    response_original <- original[[judge]]
    response_switched <- switched[[judge]]

    orig <- response_original$choice
    switch <- response_switched$choice

    # in these results, the model choosing a in one ordering and b in the
    # other means that the model was position-consistent. if the model chooses
    # the same position in both orderings, it chose two different responses.
    combined_choice <- case_when(
        orig == "a" & switch == "b" ~ "a",
        orig == "b" & switch == "a" ~ "b",
        (orig == "a" & switch == "a") | (orig == "b" & switch == "b") ~ "tie",
        TRUE ~ NA_character_
    )

    judge_res <- list(
      choice = combined_choice,
      response_original = response_original,
      response_switched = response_original
    )

    res[[judge]] <- judge_res
  }

  res
}

grade_pair_safely <- purrr::safely(grade_pair, otherwise = list())

grade_pair_impl <- function(judge, prompt) {
  response <- judge$clone()$chat(prompt)

  chose_a <- grepl("Best Response: [[A]]", response, fixed = TRUE)
  chose_b <- grepl("Best Response: [[B]]", response, fixed = TRUE)

  if (isTRUE(chose_a) && isFALSE(chose_b)) {
    return(list(response = response, choice = "a"))
  }

  if (isTRUE(chose_b) && isFALSE(chose_a)) {
    return(list(response = response, choice = "b"))
  }

  return(list(response = response, choice = NA_character_))
}

grade_pair_prompt <- function(input, target, response_a, response_b) {
  if (is.null(target)) {
    target <- "responds to the user’s request better."
  } else {
    target <- paste0(
      c("best resembles the following response: \n", target, "\n"),
      collapse = "\n"
    )
  }

  prompt <- template_pairwise
  prompt <- sub("{target}", target, prompt, fixed = TRUE)
  prompt <- sub("{input}", input, prompt, fixed = TRUE)
  prompt <- sub("{response_a}", response_a, prompt, fixed = TRUE)
  prompt <- sub("{response_b}", response_b, prompt, fixed = TRUE)

  prompt
}

# need to match up different models' responses by the input...

# scoring ----------------------------------------------------------------------
grade_queue_score <- function(x, judges) {
  cli::cli_abort("Not implemented yet.")
}

# utilities --------------------------------------------------------------------
# x is an `evals_df`
unnest_queue <- function(x) {
  x %>%
    dplyr::select(1:queue) %>%
    tidyr::unnest(queue) %>%
    tidyr::unnest_wider(queue)
}

