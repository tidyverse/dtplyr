step_colorder <- function(x, col_order) {
  stopifnot(is_step(x))
  stopifnot(is.character(col_order))

  new_step(
    parent = x,
    vars = col_order,
    col_order = col_order,
    class = "dtplyr_step_colorder"
  )
}

#' @export
dt_call.dtplyr_step_colorder <- function(x, needs_copy = x$needs_copy) {
  call2("setcolorder", dt_call(x$parent, needs_copy), x$col_order)
}
