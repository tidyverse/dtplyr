step_colorder <- function(x, col_order) {
  stopifnot(is_step(x))
  stopifnot(is.character(col_order) || is.integer(col_order))

  if (is.integer(col_order)) {
    if (all(col_order == seq_along(col_order))) {
      return(x)
    }
    vars <- x$vars[col_order]
  } else {
    if (all(col_order == x$vars)) {
      return(x)
    }
    vars <- col_order
  }

  step_call(x,
    "setcolorder",
    args = list(col_order),
    vars = vars,
    in_place = !x$implicit_copy
  )
}
