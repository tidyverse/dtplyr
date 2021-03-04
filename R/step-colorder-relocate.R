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
  out <- step_colorder(.data, new_vars)
  step_group(out, .data$groups)
}
