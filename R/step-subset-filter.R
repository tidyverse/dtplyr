
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

  if (length(dots) == 1 && is_symbol(dots[[1]])) {
    # Suppress data.table warning when filtering with a logical variable
    i <- call2("(", dots[[1]])
  } else {
    i <- Reduce(function(x, y) call2("&", x, y), dots)
  }

  step_subset_i(.data, i)
}
