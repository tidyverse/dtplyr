#' Create new columns, dropping old
#'
#' This is a method for the dplyr [transmute()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams mutate.dtplyr_step
#' @importFrom dplyr transmute any_of
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(dplyr::starwars)
#' dt %>% transmute(name, sh = paste0(species, "/", homeworld))
transmute.dtplyr_step <- function(.data, ...) {
  out <- mutate(.data, ..., .keep = "none")
  cols_expr <- names(capture_new_vars(.data, ...))
  cols_group <- group_vars(.data)
  cols_group <- setdiff(cols_group, cols_expr)
  cols_retain <- c(cols_group, cols_expr)
  select(out, any_of(cols_retain))
}
