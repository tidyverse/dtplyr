step_colorder <- function(parent, col_order) {
  stopifnot(is_step(parent))
  stopifnot(is.character(col_order))

  new_step(
    parent,
    vars = col_order,
    col_order = col_order,
    needs_copy = !parent$implicit_copy,
    class = "dtplyr_step_colorder"
  )
}

#' @export
dt_call.dtplyr_step_colorder <- function(x, needs_copy = x$needs_copy) {
  call2("setcolorder", dt_call(x$parent, needs_copy), x$col_order)
}
