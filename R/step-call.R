new_step_call <- function(parent,
                          fun,
                          args = list(),
                          vars = parent$vars,
                          groups = parent$groups
                          ) {

  stopifnot(is_step(parent))
  stopifnot(is.character(fun))
  stopifnot(is.list(args))

  new_step(
    parent = parent,
    vars = vars,
    groups = groups,
    implicit_copy = TRUE,
    fun = fun,
    args = args,
    class = "dtplyr_step_call"
  )
}

dt_call.dtplyr_step_call <- function(x, needs_copy = dt_needs_copy(x)) {
  call2(x$fun, dt_call(x$parent, needs_copy), !!!x$args)
}

# dplyr verbs -------------------------------------------------------------

#' @export
head.dtplyr_step <- function(x, n = 6L, ...) {
  new_step_call(x, "head", args = list(n = n))
}

#' @export
tail.dtplyr_step <- function(x, n = 6L, ...) {
  new_step_call(x, "tail", args = list(n = n))
}
