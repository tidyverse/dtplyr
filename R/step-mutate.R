new_step_mutate <- function(parent,
                            new_vars = list(),
                            groups = parent$groups) {

  vars <- union(parent$vars, names(new_vars))

  # TODO: analyse to break into multiple steps if needed

  new_step(
    parent,
    vars = vars,
    groups = groups,
    needs_copy = TRUE,
    new_vars = new_vars,
    class = "dtplyr_step_mutate"
  )
}

dt_call.dtplyr_step_mutate <- function(x) {
  # i is always empty because we never mutate a subset
  j <- call2(":=", !!!x$new_vars)
  out <- call2("[", dt_call(x$parent), , j)

  if (length(x$groups) > 0) {
    out$by <- call2(".", !!!syms(x$groups))
  }
  out
}

# dplyr methods -----------------------------------------------------------

mutate.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(...)

  new_step_mutate(.data, dots)
}
