step_mutate <- function(parent, new_vars = list(), use_braces = FALSE) {
  vars <- union(parent$vars, names(new_vars))
  var_is_null <- map_lgl(new_vars, is_null)
  is_last <- !duplicated(names(new_vars), fromLast = TRUE)
  vars <- setdiff(vars, names(new_vars)[var_is_null & is_last])

  new_step(
    parent,
    vars = vars,
    groups = parent$groups,
    arrange = parent$arrange,
    needs_copy = parent$needs_copy || !parent$implicit_copy,
    new_vars = new_vars,
    use_braces = use_braces,
    class = "dtplyr_step_mutate"
  )
}

dt_call.dtplyr_step_mutate <- function(x, needs_copy = x$needs_copy) {
  # i is always empty because we never mutate a subset
  if (is_empty(x$new_vars)) {
    j <- quote(.SD)
  } else if (!x$use_braces) {
    j <- call2(":=", !!!x$new_vars)
  } else {
    mutate_list <- mutate_with_braces(x$new_vars)
    j <- call2(":=", call2("c", !!!mutate_list$new_vars), mutate_list$expr)
  }

  out <- call2("[", dt_call(x$parent, needs_copy), , j)

  add_grouping_param(out, x, arrange = FALSE)
}

mutate_with_braces <- function(mutate_vars) {
  assign <- map2(syms(names(mutate_vars)), mutate_vars, function(x, y) call2("<-", x, y))
  new_vars <- unique(names(mutate_vars))
  output <- call2(".", !!!syms(new_vars))

  list(
    expr = call2("{", !!!assign, output),
    new_vars = new_vars
  )
}

# dplyr methods -----------------------------------------------------------

#' Create and modify columns
#'
#' This is a method for the dplyr [mutate()] generic. It is translated to
#' the `j` argument of `[.data.table`, using `:=` to modify "in place". If
#' `.before` or `.after` is provided, the new columns are relocated with a call
#' to [data.table::setcolorder()].
#'
#' @param .data A [lazy_dt()].
#' @param ... <[data-masking][dplyr::dplyr_data_masking]> Name-value pairs.
#'   The name gives the name of the column in the output, and the value should
#'   evaluate to a vector.
#' @param .keep `r lifecycle::badge("experimental")`
#'   Control which columns from `.data` are retained in the output. Grouping
#'   columns and columns created by `...` are always kept.
#'
#'   * `"all"` retains all columns from `.data`. This is the default.
#'   * `"used"` retains only the columns used in `...` to create new
#'     columns. This is useful for checking your work, as it displays inputs
#'     and outputs side-by-side.
#'   * `"unused"` retains only the columns _not_ used in `...` to create new
#'     columns. This is useful if you generate new columns, but no longer need
#'     the columns used to generate them.
#'   * `"none"` doesn't retain any extra columns from `.data`. Only the grouping
#'     variables and columns created by `...` are kept.
#'
#'  Note: With dtplyr `.keep` will only work with column names passed as symbols, and won't
#'  work with other workflows (e.g. `eval(parse(text = "x + 1"))`)
#' @param .before,.after \Sexpr[results=rd]{lifecycle::badge("experimental")}
#'   <[`tidy-select`][dplyr_tidy_select]> Optionally, control where new columns
#'   should appear (the default is to add to the right hand side). See
#'   [relocate()] for more details.
#' @importFrom dplyr mutate
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(data.frame(x = 1:5, y = 5:1))
#' dt %>%
#'   mutate(a = (x + y) / 2, b = sqrt(x^2 + y^2))
#'
#' # It uses a more sophisticated translation when newly created variables
#' # are used in the same expression
#' dt %>%
#'   mutate(x1 = x + 1, x2 = x1 + 1)
mutate.dtplyr_step <- function(.data, ...,
                               .keep = c("all", "used", "unused", "none"),
                               .before = NULL, .after = NULL) {
  all_dots <- capture_new_vars(.data, ...)
  trivial_dot <- imap(all_dots, ~ is_symbol(.x) && sym(.y) == .x && .y %in% .data$vars)
  dots <- all_dots[!as.vector(trivial_dot, "logical")]
  dots_list <- process_new_vars(.data, dots)
  dots <- dots_list$dots

  if (is_null(dots) || is_empty(dots)) {
    out <- .data
  } else {
    out <- step_mutate(.data, dots, dots_list$use_braces)

    .before <- enquo(.before)
    .after <- enquo(.after)
    if (!quo_is_null(.before) || !quo_is_null(.after)) {
      # Only change the order of new columns
      new <- setdiff(names(dots), .data$vars)
      out <- relocate(out, !!new, .before = !!.before, .after = !!.after)
    }

    if (dots_list$need_removal_step) {
      out <- select(out, -tidyselect::all_of(dots_list$vars_removed))
    }
  }

  .keep <- arg_match(.keep)
  if (.keep != "all") {
    cols_retain <- keep_vars(.data, out, all_dots, .keep)
    out <- select(out, tidyselect::all_of(cols_retain))
  }

  out
}

nested_vars <- function(.data, dots, all_vars) {
  new_vars <- character()
  all_new_vars <- unique(names(dots))

  init <- 0L
  for (i in seq_along(dots)) {
    cur_var <- names(dots)[[i]]
    used_vars <- all_names(get_expr(dots[[i]]))

    if (any(used_vars %in% new_vars)) {
      return(TRUE)
    } else {
      new_vars <- c(new_vars, cur_var)
    }
  }

  FALSE
}

# Helpers -----------------------------------------------------------------

all_names <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (!is.call(x)) return(NULL)

  unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
}

process_new_vars <- function(.data, dots) {
  # identify where var = NULL is being used to remove a variable
  var_is_null <- map_lgl(dots, is.null)
  is_last <- !duplicated(names(dots), fromLast = TRUE)
  var_removals <- var_is_null & is_last
  vars_removed <- names(var_removals)[var_removals]

  nested <- nested_vars(.data, dots, .data$vars)
  repeated <- anyDuplicated(names(dots))
  use_braces <- nested | repeated
  grouped <- !is_empty(group_vars(.data))
  need_removal_step <- any(var_removals) && (use_braces | grouped)
  if (need_removal_step) {
    dots <- dots[!var_removals]
  }

  list(
    dots = dots,
    use_braces = use_braces,
    need_removal_step = need_removal_step,
    vars_removed = vars_removed
  )
}

keep_vars <- function(.data, out, dots, .keep) {
  used <- unique(unlist(map(dots, all_names))) %||% character()
  used <- set_names(out$vars %in% used, out$vars)

  cols_data <- .data$vars
  cols_group <- .data$groups

  cols_expr <- names(dots)
  cols_expr_modified <- intersect(cols_expr, cols_data)
  cols_expr_new <- setdiff(cols_expr, cols_expr_modified)

  cols_used <- setdiff(cols_data, c(cols_group, cols_expr_modified, names(used)[!used]))
  cols_unused <- setdiff(cols_data, c(cols_group, cols_expr_modified, names(used)[used]))

  cols_out <- out$vars

  if (.keep == "used") {
    cols_retain <- setdiff(cols_out, cols_unused)
  } else if (.keep == "unused") {
    cols_retain <- setdiff(cols_out, cols_used)
  } else if (.keep == "none") {
    cols_retain <- setdiff(cols_out, c(cols_used, cols_unused))
  }

  cols_retain
}
