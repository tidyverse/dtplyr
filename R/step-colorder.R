step_colorder <- function(parent, col_order) {
  stopifnot(is_step(parent))
  stopifnot(is.character(col_order))

  step_call(parent,
    "setcolorder",
    args = list(col_order),
    vars = col_order,
    in_place = !parent$implicit_copy
  )
}
