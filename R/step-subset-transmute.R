#' Create new columns, dropping old
#'
#' This is a method for the dplyr [transmute()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams mutate.dtplyr_step
#' @importFrom dplyr transmute
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(dplyr::starwars)
#' dt %>% transmute(name, sh = paste0(species, "/", homeworld))
transmute.dtplyr_step <- function(.data, ...) {
  out <- mutate(.data, ..., .keep = "none")
  old_vars <- intersect(.data$vars, out$vars)
  new_vars <- setdiff(out$vars, .data$vars)
  vars <- c(old_vars, new_vars)
  select(out, all_of(vars))
}
