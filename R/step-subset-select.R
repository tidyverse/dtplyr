
#' Subset columns using their names
#'
#' This is a method for the dplyr [select()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::select
#' @importFrom dplyr select
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(data.frame(x1 = 1, x2 = 2, y1 = 3, y2 = 4))
#'
#' dt %>% select(starts_with("x"))
#' dt %>% select(ends_with("2"))
#' dt %>% select(z1 = x1, z2 = x2)
select.dtplyr_step <- function(.data, ...) {
  locs <- dtplyr_tidyselect(.data, ...)
  locs <- ensure_group_vars(locs, .data$vars, .data$groups)

  vars <- set_names(.data$vars[locs], names(locs))

  if (length(vars) == 0) {
    j <- 0L
    groups <- .data$groups
    is_unnamed <- TRUE
  } else {
    groups <- rename_groups(.data$groups, vars)
    vars <- simplify_names(vars)

    is_unnamed <- all(!have_name(vars))
    if (is_unnamed && identical(unname(vars), .data$vars)) {
      return(.data)
    }
    j <- call2(".", !!!syms(vars))
  }

  if ((.data$implicit_copy || .data$needs_copy) && is_unnamed && !can_merge_subset(.data)) {
    # Drop columns by reference if:
    #  * Data has been copied (implicitly or explicitly)
    #  * There is no renaming in the select statement
    #  * The selection can't be combined with a prior `i` step. Ex: dt[x < 7, .(x, y)]
    vars_drop <- setdiff(.data$vars, vars)
    out <- remove_vars(.data, vars_drop)
    out <- step_colorder(out, vars)
  } else {
    out <- step_subset_j(.data, vars = names(locs), groups = character(), j = j)
  }

  step_group(out, groups)
}

dtplyr_tidyselect <- function(.data, ...,
                              .call = caller_env(),
                              .allow_rename = TRUE,
                              .drop_groups = FALSE) {
  dots <- enquos(...)
  if (selection_uses_where(dots)) {
    abort("The use of `where()` is not supported by dtplyr.", call = .call)
  }
  sim_data <- simulate_vars(.data, drop_groups = .drop_groups)
  tidyselect::eval_select(expr(c(!!!dots)), sim_data, allow_rename = .allow_rename)
}

selection_uses_where <- function(x) {
  is_predicate <- unlist(lapply(x, dot_uses_where))
  any(is_predicate)
}

dot_uses_where <- function(x) {
  x <- quo_squash(x)
  if (is_call(x, "where")) {
    TRUE
  } else if (is_symbol(x) || is_atomic(x)) {
    FALSE
  } else {
    lapply(x[-1], dot_uses_where)
  }
}

simulate_vars <- function(x, drop_groups = FALSE) {
  if (drop_groups) {
    vars <- setdiff(x$vars, x$groups)
  } else {
    vars <- x$vars
  }

  as_tibble(rep_named(vars, list(logical())), .name_repair = "minimal")
}

ensure_group_vars <- function(loc, names, groups) {
  group_loc <- match(groups, names)
  missing <- setdiff(group_loc, loc)

  if (length(missing) > 0) {
    vars <- names[missing]
    inform(paste0(
      "Adding missing grouping variables: ",
      paste0("`", names[missing], "`", collapse = ", ")
    ))
    loc <- c(set_names(missing, vars), loc)
  }

  loc
}

rename_groups <- function(groups, vars) {
  old2new <- set_names(names(vars), vars)
  groups[groups %in% names(old2new)] <- old2new[groups]
  groups
}

simplify_names <- function(vars) {
  names(vars)[vars == names(vars)] <- ""
  vars
}

remove_vars <- function(.data, vars) {
  if (is_empty(vars)) {
    return(.data)
  }
  out <- step_subset(
    .data, groups = character(), j = expr(!!unique(vars) := NULL),
    vars = setdiff(.data$vars, vars)
  )
  group_by(out, !!!syms(.data$groups))
}
