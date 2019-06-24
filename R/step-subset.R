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
    class = "dtplyr_step_subset"
  )
}

dt_call.dtplyr_step_subset <- function(x) {
  i <- if (is.null(x$i)) missing_arg() else x$i
  j <- if (is.null(x$j)) missing_arg() else x$j

  if (length(x$groups) == 0) {
    call2("[", dt_call(x$parent), maybe_missing(i), maybe_missing(j))
  } else {
    by <- call2(".", !!!syms(x$groups))

    if (!is.null(x$i)) {
      j <- call2("[", expr(.SD), i, maybe_missing(j))
    }

    call2("[", dt_call(x$parent), , j, by = by)
  }
}

# dplyr methods -----------------------------------------------------------

can_merge_subset <- function(x, needs, groups = character()) {
  # Can only merge subsets
  if (!inherits(x, "dtplyr_step_subset")) {
    return(FALSE)
  }

  # If the grouping is the same
  if (!identical(groups, x$groups)) {
    return(FALSE)
  }

  switch(needs,
    "i" = is.null(x$i) && is.null(x$j),
    "j" = is.null(x$j),
    abort("Invalid needs {needs}")
  )
}

select.dtplyr_step <- function(.data, ...) {
  vars <- tidyselect::vars_select(.data$vars, ...)

  # groups <- rename_groups(groups, vars) ?
  old2new <- set_names(names(vars), vars)
  groups <- .data$groups
  groups[groups %in% names(old2new)] <- old2new[groups]

  # Strip names where not needed to simplify call
  # vars <- simplify_names(vars)
  names(vars)[vars == names(vars)] <- ""

  if (can_merge_subset(.data, "j", groups = groups)) {
    i <- .data$i
    .data <- .data$parent
  } else {
    i <- NULL
  }

  new_step_subset(
    .data,
    vars = vars,
    groups = groups,
    i = i,
    j = call2(".", !!!syms(vars))
  )
}

summarise.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(...)

  vars <- union(.data$groups, names(dots))

  if (can_merge_subset(.data, "j", groups = .data$groups)) {
    i <- .data$i
    .data <- .data$parent
  } else {
    i <- NULL
  }

  out <- new_step_subset(
    .data,
    vars = vars,
    i = i,
    j = call2(".", !!!dots)
  )

  groups <- tail(.data$groups, -1)
  new_step_group(out, groups = groups)
}

filter.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(...)

  if (length(dots) == 0) {
    i <- NULL
  } else if (length(dots) == 1) {
    i <- dots[[1]]
  } else {
    i <- call2(".", !!!dots)
  }

  new_step_subset(
    .data,
    vars = .data$vars,
    groups = .data$groups,
    i = i
  )
}
