step_call <- function(parent, fun, args = list()) {

  stopifnot(is_step(parent))
  stopifnot(is.character(fun))
  stopifnot(is.list(args))

  new_step(
    parent = parent,
    vars = parent$vars,
    groups = parent$groups,
    implicit_copy = TRUE,
    fun = fun,
    args = args,
    class = "dtplyr_step_call"
  )
}

dt_call.dtplyr_step_call <- function(x, needs_copy = x$needs_copy) {
  call2(x$fun, dt_call(x$parent, needs_copy), !!!x$args)
}

# dplyr verbs -------------------------------------------------------------

#' @export
head.dtplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "head", args = list(n = n))
}

#' @export
tail.dtplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "tail", args = list(n = n))
}

#' @importFrom dplyr distinct
#' @export
distinct.dtplyr_step <- function(.data, ..., .keep_all = FALSE) {
  dots <- capture_dots(...)

  if (length(dots) > 0) {
    only_syms <- all(vapply(dots, is_symbol, logical(1)))

    if (.keep_all) {
      if (only_syms) {
        by <- union(.data$groups, names(dots))
      } else {
        .data <- mutate(.data, !!!dots)
        by <- names(.data$new_vars)
      }
    } else {
      if (only_syms) {
        .data <- select(.data, !!!dots)
      } else {
        .data <- transmute(.data, !!!dots)
      }
      by <- NULL
    }
  } else {
    by <- NULL
  }

  args <- list()
  args$by <- by

  step_call(.data, "unique", args = args)
}
