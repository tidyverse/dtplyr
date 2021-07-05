#' Nest
#'
#' @description
#' This is a method for the tidyr [tidyr::nest()] generic. It is translated
#' using the non-nested variables in the `by` argument and `.SD` in the `j`
#' argument.
#'
#' @inheritParams tidyr::nest
#' @param data A [lazy_dt()].
#' @examples
#' if (require("tidyr", quietly = TRUE)) {
#'   dt <- lazy_dt(tibble(x = c(1, 2, 1), y = c("a", "a", "b")))
#'   dt %>% nest(data = y)
#'
#'   dt %>% dplyr::group_by(x) %>% nest()
#' }
# exported onLoad
nest.dtplyr_step <- function(.data, ..., .names_sep = NULL, .key = deprecated()) {
  if (lifecycle::is_present(.key)) {
    abort(c(
      "`nest()` for lazy data.tables doesn't support the `.key` argument.",
      i = "Use a name in the `...` argument instead."
    ))
  }

  cols <- eval_nest_dots(.data, ...)

  cols <- lapply(cols, set_names)
  if (!is.null(.names_sep)) {
    cols <- imap(cols, strip_names, .names_sep)
  }

  if (length(cols) == 1 && is.null(.names_sep)) {
    # use `.SD` as it is shorter and faster
    nm <- names(cols)
    j_exprs <- exprs(!!nm := .(.SD))
  } else {
    j_exprs <- imap(
      cols,
      function(x, name) {
        x <- simplify_names(x)
        expr(.(data.table(!!!syms(x))))
      }
    )
  }

  asis <- setdiff(.data$vars, unlist(cols))
  out <- step_subset_j(
    .data,
    vars = c(asis, names(cols)),
    j = expr(.(!!!j_exprs)),
    groups = asis,
    arrange = FALSE
  )

  groups <- intersect(out$vars, group_vars(.data))
  group_by(out, !!!syms(groups))
}

# exported onLoad
nest.data.table <- function(.data, ..., .names_sep = NULL, .key = deprecated()) {
  .data <- lazy_dt(.data)
  tidyr::nest(.data, ..., .names_sep = .names_sep, .key = .key)
}

eval_nest_dots <- function(.data, ...) {
  if (missing(...)) {
    groups <- group_vars(.data)
    if (is_empty(groups)) {
      warn(paste0(
        "`...` must not be empty for ungrouped data frames.\n",
        "Did you want `data = everything()`?"
      ))
    }

    nest_vars <- setdiff(.data$vars, groups)
    list(data = nest_vars)
  } else {
    cols <- enquos(...)
    sim_data <- simulate_vars(.data)
    lapply(cols, function(.x) names(tidyselect::eval_select(.x, sim_data)))
  }
}
