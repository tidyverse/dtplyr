step_colorder <- function(x, col_order) {
  stopifnot(is_step(x))
  stopifnot(is.character(col_order) || is.integer(col_order))

  if (any(duplicated(col_order))) {
    abort("every element of `col_order` must be unique.")
  }

  if (is.integer(col_order)) {
    if (identical(col_order, seq_along(col_order))) {
      return(x)
    }
    vars <- x$vars[col_order]
  } else {
    if (identical(col_order, x$vars)) {
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
