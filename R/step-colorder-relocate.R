#' Relocate variables using their names
#'
#' This is a method for the dplyr [relocate()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::relocate
#' @importFrom dplyr relocate
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(data.frame(x = 1, y = 2, z = 3))
#'
#' dt %>% relocate(z)
#' dt %>% relocate(y, .before = x)
#' dt %>% relocate(y, .after = y)
relocate.dtplyr_step <- function(.data, ..., .before = NULL, .after = NULL) {
  new_vars <- names(tidyselect::eval_relocate(
    expr(c(...)),
    .data,
    before = enquo(.before),
    after = enquo(.after)
  ))
  out <- step_colorder(.data, new_vars)
  step_group(out, .data$groups)
}
