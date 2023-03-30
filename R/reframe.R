#' Summarise each group to one row
#'
#' This is a method for the dplyr [reframe()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::reframe
#' @importFrom dplyr reframe
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   reframe(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
reframe.dtplyr_step <- function(.data, ..., .by = NULL) {
  out <- summarise(.data, ..., .by = {{ .by }})
  ungroup(out)
}
