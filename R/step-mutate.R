step_mutate <- function(parent, new_vars = list(), use_braces = FALSE) {
  vars <- union(parent$vars, names(new_vars))
  vars <- setdiff(vars, names(new_vars)[vapply(new_vars, is_null, lgl(1))])

  new_step(
    parent,
    vars = vars,
    groups = parent$groups,
    arrange = parent$arrange,
    needs_copy = !parent$implicit_copy,
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
                               .before = NULL, .after = NULL) {
  dots <- capture_new_vars(.data, ...)
  if (is_null(dots) | is_empty(dots)) {
    return(.data)
  }

  var_removals <- vapply(dots, is_var_removal, logical(1))
  vars_removed <- names(var_removals)[var_removals]
  nested <- nested_vars(.data, dots, .data$vars)
  repeated <- anyDuplicated(names(dots))
  use_braces <- nested | repeated
  grouped_data <- !is_empty(group_vars(.data))
  need_removal_step <- any(var_removals) && (use_braces | grouped_data)
  if (need_removal_step) {
    dots <- dots[!var_removals]
  } else {
    dots <- unmark_var_removals(dots, var_removals)  
  }
  out <- step_mutate(.data, dots, use_braces)

  .before <- enquo(.before)
  .after <- enquo(.after)
  if (!quo_is_null(.before) || !quo_is_null(.after)) {
    # Only change the order of new columns
    new <- setdiff(names(dots), .data$vars)
    out <- relocate(out, !!new, .before = !!.before, .after = !!.after)
  }

  if (need_removal_step) {
    out <- remove_vars(out, vars_removed)
  }

  out
}

#' @export
mutate.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  mutate(.data, ...)
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

is_var_removal <- function(arg) {
  inherits(arg, 'var_removal')
}

unmark_var_removals <- function(dots, var_removals) {
  map2(dots, var_removals, function(dot, is_rm) {
    if (is_rm) {
      NULL
    } else {
      dot
    }
  })
}

