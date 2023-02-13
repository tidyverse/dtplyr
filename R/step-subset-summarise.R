
#' Summarise each group to one row
#'
#' This is a method for the dplyr [summarise()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::summarise
#' @importFrom dplyr summarise
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   summarise(vs = mean(vs))
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   summarise(across(disp:wt, mean))
summarise.dtplyr_step <- function(.data, ..., .by = NULL, .groups = NULL) {
  dots <- capture_dots(.data, ...)
  check_summarise_vars(dots)

  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")
  if (by$uses_by) {
    group_vars <- by$names
    .groups <- "drop"
  } else {
    group_vars <- .data$groups
  }

  if (length(dots) == 0) {
    if (length(group_vars) == 0) {
      out <- step_subset_j(.data, vars = character(), j = 0L)
    } else {
      # Acts like distinct on grouping vars
      out <- distinct(.data, !!!syms(group_vars))
    }
  } else {
    out <- step_subset_j(
      .data,
      vars = union(group_vars, names(dots)),
      j = call2(".", !!!dots),
      by = by
    )
  }

  replaced_group_vars <- intersect(group_vars, names(dots))
  if (!is_empty(replaced_group_vars)) {
    out <- step_subset(
      out,
      groups = character(),
      j = expr(!!replaced_group_vars := NULL)
    )
  }

  out_groups <- summarise_groups(.data, .groups, caller_env())
  step_group(out, groups = out_groups)
}


# For each expression, check if it uses any newly created variables
check_summarise_vars <- function(dots) {
  for (i in seq_along(dots)) {
    used_vars <- all_names(get_expr(dots[[i]]))
    cur_vars <- names(dots)[seq_len(i - 1)]

    if (any(used_vars %in% cur_vars)) {
      abort(paste0(
        "`", names(dots)[[i]], "` ",
        "refers to a variable created earlier in this summarise().\n",
        "Do you need an extra mutate() step?"
      ), call = caller_env())
    }
  }
}

summarise_groups <- function(.data, .groups, env_caller) {
  if (!is.null(.groups) && !.groups %in% c("drop_last", "drop", "keep")) {
    abort(c(
      paste0(
        "`.groups` can't be ", as_label(.groups),
        if (.groups == "rowwise") " in dtplyr"
      ),
      i = 'Possible values are NULL (default), "drop_last", "drop", and "keep"'
    ), call = caller_env())
  }

  group_vars <- .data$groups
  n <- length(group_vars)

  verbose <- summarise_verbose(.groups, env_caller)
  if (verbose && n > 1) {
    new_groups <- glue::glue_collapse(paste0("'", group_vars[-n], "'"), sep = ", ")
    summarise_inform("has grouped output by {new_groups}")
  }

  .groups <- .groups %||% "drop_last"
  switch(.groups,
    drop_last = group_vars[-n],
    keep = group_vars,
    drop = character()
  )
}

summarise_verbose <- function(.groups, .env) {
  is.null(.groups) &&
    is_reference(topenv(.env), global_env()) &&
    !identical(getOption("dplyr.summarise.inform"), FALSE)
}

summarise_inform <- function(..., .env = parent.frame()) {
  inform(paste0(
    "`summarise()` ", glue::glue(..., .envir = .env), '. You can override using the `.groups` argument.'
  ))
}
