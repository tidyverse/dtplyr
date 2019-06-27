#' Group by with ordering
#'
#' Uses [data.table::setkey()]; this guarantees that the output is sorted
#' and typically increases performance when peforming multiple operations on
#' the same grouping.
#'
#' @param .data A [lazy_dt]
#' @param ... Variables to key by
#' @export
#' @examples
#' library(dplyr)
#' library(data.table)
#'
#' mtcars2 <- lazy_dt(mtcars)
#'
#' # group_by() doesn't order by group
#' mtcars2 %>% group_by(cyl) %>% summarise(mpg = mean(mpg))
#'
#' # key_by() does
#' mtcars2 %>% key_by(cyl) %>% summarise(mpg = mean(mpg))
key_by <- function(.data, ...) {
  stopifnot(is_step(.data))

  dots <- unname(ensyms(...))
  cols <- vapply(dots, as.character, character(1))

  out <- step_call(.data, "setkeyv", list(cols = cols), in_place = TRUE)
  step_group(out, cols)
}
