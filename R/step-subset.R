new_step_subset <- function(parent,
                            vars = parent$vars,
                            groups = parent$groups,
                            i = NULL,
                            j = NULL
                            ) {

  stopifnot(is_step(parent))
  stopifnot(is.null(i) || is_expression(i))
  stopifnot(is.null(j) || is_expression(j))

  new_step(
    parent = parent,
    vars = vars,
    groups = groups,
    i = i,
    j = j,
    implicit_copy = is.null(i) || is.null(j),
    class = "dtplyr_step_subset"
  )
}

# When adding a subset that contains only j, it may be possible to merge
# the previous step.
step_subset_j <- function(parent,
                              vars = parent$vars,
                              groups = parent$groups,
                              j = NULL) {
  if (can_merge_subset(parent)) {
    i <- parent$i
    parent <- parent$parent
  } else {
    i <- NULL
  }

  new_step_subset(
    parent,
    vars = vars,
    groups = groups,
    i = i,
    j = j
  )
}

can_merge_subset <- function(x) {
  # Can only merge subsets
  if (!inherits(x, "dtplyr_step_subset")) {
    return(FALSE)
  }

  # Don't need to check that groups are identical because the only
  # dplyr functions that generate expression in i are
  # filter/slice/sample/arrange/join and don't affect groups

  is.null(x$j)
}

dt_call.dtplyr_step_subset <- function(x, needs_copy = dt_needs_copy(x)) {
  i <- if (is.null(x$i)) missing_arg() else x$i
  j <- if (is.null(x$j)) missing_arg() else x$j

  if (length(x$groups) == 0) {
    call2("[", dt_call(x$parent, needs_copy), maybe_missing(i), maybe_missing(j))
  } else {
    by <- call2(".", !!!syms(x$groups))

    if (!is.null(x$i)) {
      j <- call2("[", expr(.SD), i, maybe_missing(j))
    }

    call2("[", dt_call(x$parent, needs_copy), , j, by = by)
  }
}

# dplyr methods -----------------------------------------------------------

#' @export
select.dtplyr_step <- function(.data, ...) {
  vars <- tidyselect::vars_select(.data$vars, ...)

  groups <- rename_groups(.data$groups, vars)
  vars <- simplify_names(vars)

  step_subset_j(
    .data,
    vars = vars,
    groups = groups,
    j = call2(".", !!!syms(vars))
  )
}

#' @export
rename.dtplyr_step <- function(.data, ...) {
  vars <- tidyselect::vars_rename(.data$vars, ...)

  groups <- rename_groups(.data$groups, vars)
  vars <- simplify_names(vars)

  step_subset_j(
    .data,
    vars = vars,
    groups = groups,
    j = call2(".", !!!syms(vars))
  )
}

#' @export
summarise.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(...)

  out <- step_subset_j(
    .data,
    vars = union(.data$groups, names(dots)),
    j = call2(".", !!!dots)
  )
  new_step_group(out, groups = tail(.data$groups, -1))
}

# exported onLoad
filter.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(...)

  i <- Reduce(function(x, y) call2("&", x, y), dots)
  new_step_subset(.data, i = i)
}

#' @export
arrange.dtplyr_step <- function(.data, ..., .by_group = FALSE) {
  dots <- capture_dots(...)
  if (.by_group) {
    dots <- c(syms(.data$groups), dots)
  }

  # Order without grouping then restore
  step <- new_step_subset(.data, i = call2("order", !!!dots), groups = character())
  new_step_group(step, groups = .data$groups)
}

# helpers ------------------------------------------------------------------

rename_groups <- function(groups, vars) {
  old2new <- set_names(names(vars), vars)
  groups[groups %in% names(old2new)] <- old2new[groups]
  groups
}

simplify_names <- function(vars) {
  names(vars)[vars == names(vars)] <- ""
  vars
}
