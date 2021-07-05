
#' Summarise each group to one row
#'
#' This is a method for the dplyr [summarise()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::summarise
#' @importFrom dplyr summarise
#' @param .groups \Sexpr[results=rd]{lifecycle::badge("experimental")} Grouping structure of the result.
#'
#'   * "drop_last": dropping the last level of grouping. This was the
#'   only supported option before version 1.0.0.
#'   * "drop": All levels of grouping are dropped.
#'   * "keep": Same grouping structure as `.data`.
#'
#'   When `.groups` is not specified, it defaults to "drop_last".
#'
#'   In addition, a message informs you of that choice, unless the result is ungrouped,
#'   the option "dplyr.summarise.inform" is set to `FALSE`,
#'   or when `summarise()` is called from a function in a package.
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
summarise.dtplyr_step <- function(.data, ..., .groups = NULL) {
  dots <- capture_dots(.data, ...)
  check_summarise_vars(dots)

  if (length(dots) == 0) {
    if (length(.data$groups) == 0) {
      out <- step_subset_j(.data, vars = character(), j = 0L)
    } else {
      # Acts like distinct on grouping vars
      out <- distinct(.data, !!!syms(.data$groups))
    }
  } else {
    out <- step_subset_j(
      .data,
      vars = union(.data$groups, names(dots)),
      j = call2(".", !!!dots)
    )
  }

  out_groups <- summarise_groups(.data, .groups, caller_env())
  step_group(out, groups = out_groups)
}

#' @export
summarise.data.table <- function(.data, ..., .groups = NULL) {
  .data <- lazy_dt(.data)
  summarise(.data, ..., .groups = .groups)
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
      ))
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
    ))
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
