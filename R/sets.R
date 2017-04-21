
# Set operations ---------------------------------------------------------------

#' @rawNamespace
#' if (utils::packageVersion("dplyr") > "0.5.0") {
#'   importFrom(dplyr,distinct)
#'   S3method(distinct,data.table)
#' }
distinct.data.table <- function(.data, ..., .keep_all = FALSE) {
  distinct_(.data, .dots = lazyeval::lazy_dots(...), .keep_all = .keep_all)
}

#' @export
#' @importFrom dplyr distinct_
distinct_.data.table <- function(.data, ..., .dots, .keep_all = FALSE) {
  dist <- distinct_vars(.data, ..., .dots = .dots)

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
distinct_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

distinct_vars <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  # If any calls, use mutate to add new columns, then distinct on those
  needs_mutate <- vapply(dots, function(x) !is.name(x$expr), logical(1))
  if (any(needs_mutate)) {
    .data <- mutate_(.data, .dots = dots[needs_mutate])
  }

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use their names
  list(data = .data, vars = names(dots))
}

