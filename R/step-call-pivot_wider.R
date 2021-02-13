#' Pivot data from long to wide
#'
#' @description
#' This is a method for the tidyr [pivot_wider()] generic. It is translated
#' [data.table::dcast()]
#'
#' @param data A [lazy_dt()].
#' @inheritParams tidyr::pivot_wider
#' @importFrom tidyr pivot_wider
#' @export
#' @examples
#' library(tidyr)
#'
#' fish_encounters_dt <- lazy_dt(fish_encounters)
#' fish_encounters_dt
#' fish_encounters_dt %>%
#'   pivot_wider(names_from = station, values_from = seen)
#' # Fill in missing values
#' fish_encounters_dt %>%
#'   pivot_wider(names_from = station, values_from = seen, values_fill = 0)
#'
#' # Generate column names from multiple variables
#' us_rent_income_dt <- lazy_dt(us_rent_income)
#' us_rent_income_dt
#' us_rent_income_dt %>%
#'   pivot_wider(names_from = variable, values_from = c(estimate, moe))
#'
#' # When there are multiple `names_from` or `values_from`, you can use
#' # use `names_sep` or `names_glue` to control the output variable names
#' us_rent_income_dt %>%
#'   pivot_wider(
#'     names_from = variable,
#'     names_sep = ".",
#'     values_from = c(estimate, moe)
#'   )
#' us_rent_income_dt %>%
#'   pivot_wider(
#'     names_from = variable,
#'     names_glue = "{variable}_{.value}",
#'     values_from = c(estimate, moe)
#'   )
#'
#' # Can perform aggregation with values_fn
#' warpbreaks_dt <- lazy_dt(as_tibble(warpbreaks[c("wool", "tension", "breaks")]))
#' warpbreaks_dt
#' warpbreaks_dt %>%
#'   pivot_wider(
#'     names_from = wool,
#'     values_from = breaks,
#'     values_fn = mean
#'   )
pivot_wider.dtplyr_step <- function(data,
                                    id_cols = NULL,
                                    names_from = name,
                                    names_prefix = "",
                                    names_sep = "_",
                                    names_glue = NULL,
                                    names_sort = FALSE,
                                    names_repair = "check_unique",
                                    values_from = value,
                                    values_fill = NULL,
                                    values_fn = NULL,
                                    ...) {

  sim_data <- simulate_vars(data)

  names_from <- names(tidyselect::eval_select(enquo(names_from), sim_data))
  values_from <- names(tidyselect::eval_select(enquo(values_from), sim_data))

  id_cols <- enquo(id_cols)

  if (quo_is_null(id_cols)) {
    sim_vars <- names(sim_data)
    id_cols <- sim_vars[!sim_vars %in% c(names_from, values_from)]
  } else {
    id_cols <- names(tidyselect::eval_select(id_cols, sim_data))
  }

  if (length(names_from) > 1) {
    new_vars <- mutate(data, .names_from = paste(!!!syms(names_from), sep = names_sep))
    new_vars <- unique(pull(new_vars, .names_from))
  } else {
    new_vars <- unique(pull(data, !!sym(names_from)))
  }

  if (!is.null(names_glue)) {
    glue_vars <- mutate(data, .names_from = glue(names_glue, .envir = .SD))
    glue_vars <- unique(pull(glue_vars, .names_from))
  }

  if (length(values_from) > 1) {
    new_vars <- lapply(values_from, function(.x) paste(.x, new_vars, sep = names_sep))
    new_vars <- unlist(new_vars)
  }

  no_id <- length(id_cols) == 0

  if (no_id) {
    lhs <- "..."
    new_vars <- c(".", new_vars)
  } else {
    lhs <- paste(id_cols, collapse = " + ")
  }

  vars <- c(id_cols, new_vars)

  rhs <- paste(names_from, collapse = " + ")

  dcast_form <- paste(lhs, rhs, sep = " ~ ")

  args <- list(
    formula = dcast_form,
    value.var = values_from,
    fun.aggregate = values_fn,
    sep = names_sep,
    fill = values_fill
  )

  # Clean up call args if defaults are used
  args <- args[!vapply(args, is.null, lgl(1))]

  if (names_sep == "_") {
    args$sep <- NULL
  }

  out <- step_call(data, "dcast", args = args, vars = vars)

  if (no_id) {
    out <- select(out, -.)

    new_vars <- new_vars[new_vars != "."]
  }

  if (!is.null(names_glue)) {
    out <- step_call(
      out,
      "setnames",
      args = list(old = new_vars, new = glue_vars),
      vars = c(id_cols, glue_vars),
      in_place = FALSE
    )

    # In case of names_sort = TRUE
    new_vars <- glue_vars
  }

  if (nchar(names_prefix) > 0 && is.null(names_glue)) {
    new_names <- paste0(names_prefix, new_vars)

    out <- step_call(
      out,
      "setnames",
      args = list(old = new_vars, new = new_names),
      vars = c(id_cols, new_names),
      in_place = FALSE
    )

    # In case of names_sort = TRUE
    new_vars <- new_names
  }

  if (names_sort) {
    cols_sorted <- c(id_cols, sort(new_vars))

    out <- step_call(
      out,
      "setcolorder",
      args = list(cols_sorted),
      vars = cols_sorted,
      in_place = FALSE
    )
  }

  out <- step_repair(out, repair = names_repair)

  out
}

#' @export
pivot_wider.data.table <- function(data,
                                   id_cols = NULL,
                                   names_from = name,
                                   names_prefix = "",
                                   names_sep = "_",
                                   names_glue = NULL,
                                   names_sort = FALSE,
                                   names_repair = "check_unique",
                                   values_from = value,
                                   values_fill = NULL,
                                   values_fn = NULL,
                                   ...) {
  data <- lazy_dt(data)

  pivot_wider(
    data,
    id_cols = {{ id_cols }},
    names_from = {{ names_from }},
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_glue = names_glue,
    names_sort = names_sort,
    names_repair = names_repair,
    values_from = {{ values_from }},
    values_fn = values_fn
  )
}

step_repair <- function(data, repair = "check_unique", in_place = TRUE) {
  sim_data <- simulate_vars(data)
  data_names <- names(sim_data)
  repaired_names <- vec_as_names(data_names, repair = repair)

  if (any(data_names != repaired_names)) {
    data <- step_call(
      data,
      "setnames",
      args = list(new = repaired_names),
      vars = repaired_names,
      in_place = in_place
    )
  }

  data
}
