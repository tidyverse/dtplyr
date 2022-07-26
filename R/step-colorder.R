step_colorder <- function(x, col_order) {
  stopifnot(is_step(x))
  stopifnot(is.character(col_order) || is.integer(col_order))

  if (any(duplicated(col_order))) {
    abort("Every element of `col_order` must be unique.")
  }

  col_order <- unname(col_order)
  if (is.integer(col_order)) {
    if (identical(col_order, seq_along(col_order))) {
      return(x)
    }
    vars <- x$vars[col_order]
  } else {
    vars_selected <- x$vars[x$vars %in% col_order]
    vars_count <- vctrs::vec_count(vars_selected)
    vars_problematic <- vars_count$key[vars_count$count != 1]
    if (!is_empty(vars_problematic)) {
      vars_error <- paste0(vars_problematic, collapse = ", ")
      msg <- paste0("The column(s) ", vars_error, " do not uniquely match a column in `x`.")
      abort(msg)
    }

    if (identical(col_order, x$vars[seq_along(col_order)])) {
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
