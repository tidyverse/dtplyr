step_call <- function(parent, fun, args = list(), vars = parent$vars, in_place = FALSE) {

  stopifnot(is_step(parent))
  stopifnot(is.character(fun))
  stopifnot(is.list(args))

  new_step(
    parent = parent,
    vars = vars,
    groups = parent$groups,
    implicit_copy = !in_place,
    needs_copy = in_place || parent$needs_copy,
    fun = fun,
    args = args,
    class = "dtplyr_step_call"
  )
}

dt_call.dtplyr_step_call <- function(x, needs_copy = x$needs_copy) {
  call2(x$fun, dt_call(x$parent, needs_copy), !!!x$args)
}

# dplyr verbs -------------------------------------------------------------

#' @importFrom utils head
#' @export
head.dtplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "head", args = list(n = n))
}

#' @importFrom utils tail
#' @export
tail.dtplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "tail", args = list(n = n))
}

#' @importFrom dplyr rename
#' @export
rename.dtplyr_step <- function(.data, ...) {
  sim_data <- simulate_vars(.data)
  locs <- tidyselect::eval_rename(expr(c(...)), sim_data)

  new_vars <- .data$vars
  new_vars[locs] <- names(locs)

  vars <- set_names(.data$vars[locs], names(locs))
  vars <- vars[vars != names(vars)]

  if (length(vars) == 0) {
    return(.data)
  }

  out <- step_call(.data,
    "setnames",
    args = list(unname(vars), names(vars)),
    vars = new_vars,
    in_place = TRUE
  )

  groups <- rename_groups(.data$groups, vars)
  step_group(out, groups)
}


#' @importFrom dplyr distinct
#' @export
distinct.dtplyr_step <- function(.data, ..., .keep_all = FALSE) {
  dots <- capture_dots(.data, ...)

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


#' @export
unique.dtplyr_step <- function(x, incomparables = FALSE, ...) {
  if (!missing(incomparables)) {
    abort("`incomparables` not supported by `unique.dtplyr_step()`")
  }
  distinct(x)
}
