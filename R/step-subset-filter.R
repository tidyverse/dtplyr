
#' Subset rows using column values
#'
#' This is a method for the dplyr [arrange()] generic. It is translated to
#' the `i` argument of `[.data.table`
#'
#' @param .data A [lazy_dt()].
#' @param .preserve Ignored
#' @inheritParams dplyr::filter
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#' dt %>% filter(cyl == 4)
#' dt %>% filter(vs, am)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   filter(mpg > mean(mpg))
#' @importFrom dplyr filter
# exported onLoad
filter.dtplyr_step <- function(.data, ..., .preserve = FALSE) {
  dots <- capture_dots(.data, ..., .j = FALSE)

  if (filter_by_lgl_col(dots)) {
    # Suppress data.table warning when filtering with a logical variable
    i <- call2("(", dots[[1]])
  } else {
    i <- Reduce(function(x, y) call2("&", x, y), dots)
  }

  step_subset_i(.data, i)
}

filter_by_lgl_col <- function(dots) {
  if (length(dots) > 1) {
    return(FALSE)
  }

  dot <- dots[[1]]
  if (is_symbol(dot)) {
    return(TRUE)
  }

  # catch expressions of form `!x`
  is_call(dot, name = "!", n = 1) && is_symbol(dot[[2]])
}

# exported onLoad
filter.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  filter(.data, ...)
}
