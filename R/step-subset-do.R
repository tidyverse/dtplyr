#' @importFrom dplyr do
#' @export
do.dtplyr_step <- function(.data, ...) {
  # This is a partial implementation, because I don't think that many
  # people are likely to use it, given that do() is marked as questioning
  # Problems:
  # * doesn't handle unnamed case
  # * doesn't set .SDcols so `.SD` will only refer to non-groups
  # * can duplicating group vars (#5)

  dots <- capture_dots(.data, ...)

  if (any(names2(dots) == "")) {
    # I can't see any way to figure out what the variables are
    abort("Unnamed do() not supported by dtplyr")
  }

  new_vars <- lapply(dots, function(x) call2(".", x))
  j <- call2(".", !!!new_vars)

  vars <- union(.data$vars, names(dots))

  step_subset_j(.data, vars = vars, j = j)
}

#' @export
do.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  do(.data, ...)
}
