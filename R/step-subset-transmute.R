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

  var_removals <- vapply(dots, is_var_removal, logical(1))
  vars_removed <- names(var_removals)[var_removals]
  nested_vars <- nested_vars(.data, dots, .data$vars)
  repeated_vars <- anyDuplicated(names(dots))
  use_braces <- nested_vars | repeated_vars
  groups <- group_vars(.data)
  grouped_data <- !is_empty(groups)
  need_removal_step <- any(var_removals) && (use_braces | grouped_data)

  if (need_removal_step) {
    dots <- dots[!var_removals]
  } else {
    dots <- unmark_var_removals(dots, var_removals)  
  }

  if (grouped_data) {
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

  if (!use_braces) {
    j <- call2(".", !!!dots)
  } else {
    j <- mutate_nested_vars(dots)$expr
  }
  vars <- union(group_vars(.data), names(dots))
  out <- step_subset_j(.data, vars = vars, j = j)
  if (need_removal_step) {
    out <- remove_vars(out, vars_removed)
  }

  out
}

#' @export
transmute.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  transmute(.data, ...)
}
