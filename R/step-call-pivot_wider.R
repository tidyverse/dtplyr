#' Pivot data from long to wide
#'
#' @description
#' This is a method for the tidyr `pivot_wider()` generic. It is translated to
#' [data.table::dcast()]
#'
#' @param data A [lazy_dt()].
#' @inheritParams tidyr::pivot_wider
#' @param values_fn A function, the default is `length()`. Note this is different
#' behavior than `tidyr::pivot_wider()`, which returns a list column by default.
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
# exported onLoad
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
  names_from <- names(tidyselect::eval_select(enquo(names_from), data))
  values_from <- names(tidyselect::eval_select(enquo(values_from), data))

  id_cols <- enquo(id_cols)
  if (quo_is_null(id_cols)) {
    id_cols <- setdiff(data$vars, c(names_from, values_from))
  } else {
    id_cols <- names(tidyselect::eval_select(id_cols, data))
  }

  if (length(names_from) > 1) {
    new_vars <- mutate(shallow_dt(data), .names_from = paste(!!!syms(names_from), sep = names_sep))
    new_vars <- unique(pull(new_vars, .names_from))
  } else {
    new_vars <- unique(pull(data, !!sym(names_from)))
    new_vars <- as.character(new_vars)
  }

  if (!is.null(names_glue)) {
    glue_df <- as.data.table(distinct(ungroup(data), !!!syms(names_from)))
    glue_df <- vctrs::vec_rep(glue_df, length(values_from))
    glue_df$.value <- vctrs::vec_rep_each(values_from, length(new_vars))
    glue_vars <- as.character(glue::glue_data(glue_df, names_glue))
  }

  if (length(values_from) > 1) {
    new_vars <- lapply(values_from, function(.x) paste(.x, new_vars, sep = names_sep))
    new_vars <- unlist(new_vars)
  }

  no_id <- length(id_cols) == 0
  if (no_id) {
    lhs <- "..." # using symbol causes dcast() to fail
    new_vars <- c(".", new_vars)
  } else {
    lhs <- call_reduce(syms(id_cols), "+")
  }

  rhs <- call_reduce(syms(names_from), "+")

  vars <- c(id_cols, new_vars)

  args <- list(
    formula = call2("~", lhs, rhs),
    value.var = values_from,
    fun.aggregate = values_fn,
    sep = names_sep,
    fill = values_fill
  )

  # Clean up call args if defaults are used
  args <- args[!map_lgl(args, is.null)]

  if (names_sep == "_") {
    args$sep <- NULL
  }

  out <- step_call(data, "dcast", args = args, vars = vars)

  if (no_id && names_sort) {
    new_vars <- new_vars[new_vars != "."]
    cols_sorted <- sort(new_vars)
    out <- select(out, !!!syms(cols_sorted))
  } else if (no_id) {
    new_vars <- new_vars[new_vars != "."]
    out <- select(out, -.)
  }

  if (!is.null(names_glue)) {
    out <- step_setnames(out, new_vars, glue_vars, in_place = FALSE)

    # In case of names_sort = TRUE
    new_vars <- glue_vars
  } else if (nchar(names_prefix) > 0) {
    new_names <- paste0(names_prefix, new_vars)
    out <- step_setnames(out, new_vars, new_names, in_place = FALSE)

    # In case of names_sort = TRUE
    new_vars <- new_names
  }

  if (names_sort && !no_id) {
    cols_sorted <- c(id_cols, sort(new_vars))

    out <- step_colorder(out, cols_sorted)
  }

  out <- step_repair(out, repair = names_repair)

  out
}

globalVariables(c(".", ".names_from", "name", "value", "pivot_wider"))

step_repair <- function(data, repair = "check_unique", in_place = TRUE) {
  sim_data <- simulate_vars(data)
  data_names <- names(sim_data)
  repaired_names <- vctrs::vec_as_names(data_names, repair = repair)

  if (any(data_names != repaired_names)) {
    data <- step_setnames(data, seq_along(data_names), repaired_names, in_place = in_place)
  }

  data
}

shallow_dt <- function(x) {
  filter(x, TRUE)
}

call_reduce <- function(x, fun) {
  Reduce(function(x, y) call2(fun, x, y), x)
}
