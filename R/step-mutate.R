step_mutate <- function(parent, new_vars = list(), nested = FALSE) {
  vars <- union(parent$vars, names(new_vars))
  vars <- setdiff(vars, names(new_vars)[vapply(new_vars, is_null, lgl(1))])

  new_step(
    parent,
    vars = vars,
    groups = parent$groups,
    arrange = parent$arrange,
    needs_copy = !parent$implicit_copy,
    new_vars = new_vars,
    nested = nested,
    class = "dtplyr_step_mutate"
  )
}

dt_call.dtplyr_step_mutate <- function(x, needs_copy = x$needs_copy) {
  # i is always empty because we never mutate a subset
  if (!x$nested) {
    j <- call2(":=", !!!x$new_vars)
  } else {
    mutate_list <- mutate_nested_vars(x$new_vars)
    j <- call2(":=", call2("c", !!!mutate_list$new_vars), mutate_list$expr)
  }

  out <- call2("[", dt_call(x$parent, needs_copy), , j)

  add_grouping_param(out, x, arrange = FALSE)
}

mutate_nested_vars <- function(mutate_vars) {
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
#' the `j` argument of `[.data.table`, using `:=` to modify "in place".
#'
#' @param .data A [lazy_dt()].
#' @param ... <[data-masking][dplyr::dplyr_data_masking]> Name-value pairs.
#'   The name gives the name of the column in the output, and the value should
#'   evaluate to a vector.
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
mutate.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(.data, ...)
  if (is_null(dots)) {
    return(.data)
  }

  nested <- nested_vars(.data, dots, .data$vars)
  step_mutate(.data, dots, nested)
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
