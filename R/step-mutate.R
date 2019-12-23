step_mutate <- function(parent, new_vars = list(), nested = FALSE) {
  vars <- union(parent$vars, names(new_vars))

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
    assign <- Map(function(x, y) call2("<-", x, y), syms(names(x$new_vars)), x$new_vars)
    output <- call2(".", !!!syms(names(x$new_vars)))
    expr <- call2("{", !!!assign, output)
    j <- call2(":=", call2("c", !!!names(x$new_vars)), expr)
  }

  out <- call2("[", dt_call(x$parent, needs_copy), , j)

  add_grouping_param(out, x)
}

# dplyr methods -----------------------------------------------------------

#' @importFrom dplyr mutate
#' @export
mutate.dtplyr_step <- function(.data, ...) {
  if (missing(...)) {
    return(.data)
  }

  dots <- capture_dots(.data, ...)

  nested <- nested_vars(.data, dots, .data$vars)
  step_mutate(.data, dots, nested)
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
