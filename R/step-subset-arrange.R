#' Arrange rows by column values
#'
#' This is a method for dplyr generic [arrange()]. It is translated to
#' an [order()] call in the `i` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::arrange
#' @importFrom dplyr arrange
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#' dt %>% arrange(vs, cyl)
#' dt %>% arrange(desc(vs), cyl)
#' dt %>% arrange(across(mpg:disp))
arrange.dtplyr_step <- function(.data, ..., .by_group = FALSE) {
  dots <- capture_dots(.data, ..., .j = FALSE)
  if (.by_group) {
    dots <- c(syms(.data$groups), dots)
  }

  if (length(dots) == 0) {
    return(.data)
  }

  # Order without grouping then restore
  dots <- set_names(dots, NULL)
  step <- step_subset(.data, i = call2("order", !!!dots), groups = character())
  step_group(step, groups = .data$groups)
}

#' @export
arrange.data.table <- function(.data, ..., .by_group = FALSE) {
  .data <- lazy_dt(.data)
  arrange(.data, ..., .by_group = .by_group)
}
