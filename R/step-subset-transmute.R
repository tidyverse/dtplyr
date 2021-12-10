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
  dots <- capture_new_vars(.data, ...)
  to_remove <- vapply(dots, is_zap, lgl(1))
  dots <- dots[!to_remove]
  nested <- nested_vars(.data, dots, .data$vars)

  groups <- group_vars(.data)
  if (!is_empty(groups)) {
    # TODO could check if there is actually anything mutated, e.g. to avoid
    # DT[, .(x = x)]
    is_group_var <- names(dots) %in% groups
    group_dots <- dots[is_group_var]

    .data <- mutate(ungroup(.data), !!!group_dots)
    .data <- group_by(.data, !!!syms(groups))

    dots <- dots[!is_group_var]
  }

  if (is_empty(dots)) {
    # grouping variables have been removed from `dots` so `select()` would
    # produce a message "Adding grouping vars".
    # As `dplyr::transmute()` doesn't generate a message when adding group vars
    # we can also leave it away here
    return(select(.data, !!!group_vars(.data)))
  }

  if (!nested) {
    j <- call2(".", !!!dots)
  } else {
    j <- mutate_nested_vars(dots)$expr
  }
  vars <- union(group_vars(.data), names(dots))
  out <- step_subset_j(.data, vars = vars, j = j)
  remove_vars(out, names(to_remove)[to_remove])
}

#' @export
transmute.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  transmute(.data, ...)
}
