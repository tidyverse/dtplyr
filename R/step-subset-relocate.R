
#' @importFrom dplyr relocate
#' @export
relocate.dtplyr_step <- function(.data, ..., .before = NULL, .after = NULL) {
  sim_data <- simulate_vars(.data)
  to_move <- tidyselect::eval_select(expr(c(...)), sim_data)

  if (length(to_move) == 0) {
    return(.data)
  }

  .before <- enquo(.before)
  .after <- enquo(.after)
  has_before <- !quo_is_null(.before)
  has_after <- !quo_is_null(.after)

  if (has_before && has_after) {
    abort("Must supply only one of `.before` and `.after`.")
  } else if (has_before) {
    where <- min(unname(tidyselect::eval_select(.before, sim_data)))
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  } else if (has_after) {
    where <- max(unname(tidyselect::eval_select(.after, sim_data)))
    if (!where %in% to_move) {
      to_move <- c(where, to_move)
    }
  } else {
    where <- 1L
    if (!where %in% to_move) {
      to_move <- union(to_move, where)
    }
  }

  lhs <- setdiff(seq2(1, where - 1), to_move)
  rhs <- setdiff(seq2(where + 1, ncol(.data)), to_move)
  new_vars <- .data$vars[unique(c(lhs, to_move, rhs))]
  j <- call2(".", !!!syms(new_vars))
  out <- step_subset_j(.data, vars = new_vars, groups = character(), j = j)
  step_group(out, .data$groups)
}
