
# Set operations ---------------------------------------------------------------

#' @importFrom dplyr distinct
#' @export
distinct.data.table <- function(.data, ..., .dots, .keep_all = FALSE) {
  dist <- distinct_vars(.data, quos(..., .named = TRUE), .keep_all = .keep_all)

  if (length(dist$vars) == 0) {
    res <- unique(dist$data, by = NULL)
  } else {
    res <- unique(dist$data, by = dist$vars)
  }

  if (length(dist$vars) > 0 && !.keep_all) {
    res <- res[, dist$vars, with = FALSE]
  }

  res
}
#' @export
distinct.tbl_dt <- function(.data, ...) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# unexported from dplyr, removed purrr dependency
distinct_vars <- function(.data, vars, group_vars = character(), .keep_all = FALSE) {
  stopifnot(is_quosures(vars), is.character(group_vars))

  # If no input, keep all variables
  if (length(vars) == 0) {
    return(list(
      data = .data,
      vars = names(.data),
      keep = names(.data)
    ))
  }

  # If any calls, use mutate to add new columns, then distinct on those
  needs_mutate <- vapply(vars, quo_is_lang, TRUE)
  if (any(needs_mutate)) {
    .data <- mutate(.data, !!! vars[needs_mutate])
  }

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  out_vars <- intersect(names(.data), c(names(vars), group_vars))

  if (.keep_all) {
    keep <- names(.data)
  } else {
    keep <- unique(out_vars)
  }

  list(data = .data, vars = out_vars, keep = keep)
}
