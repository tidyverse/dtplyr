step_call <- function(parent, fun, args = list(), in_place = FALSE) {

  stopifnot(is_step(parent))
  stopifnot(is.character(fun))
  stopifnot(is.list(args))

  new_step(
    parent = parent,
    vars = parent$vars,
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

#' @export
head.dtplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "head", args = list(n = n))
}

#' @export
tail.dtplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "tail", args = list(n = n))
}

#' @importFrom dplyr rename
#' @export
rename.dtplyr_step <- function(.data, ...) {
  vars <- tidyselect::vars_rename(.data$vars, ...)
  vars <- vars[vars != names(vars)]

  if (length(vars) == 0) {
    return(.data)
  }

  out <- step_call(.data,
    "setnames",
    args = list(unname(vars), names(vars)),
    in_place = TRUE
  )

  groups <- rename_groups(.data$groups, vars)
  step_group(out, groups)
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

#' Group by with ordering
#'
#' Uses [data.table::setkey()]; this guarantees that the output is sorted
#' and typically increases performance when peforming multiple operations on
#' the same grouping.
#'
#' @param .data A [lazy_dt]
#' @param ... Variables to key by
#' @export
#' @examples
#' library(dplyr)
#' library(data.table)
#'
#' mtcars2 <- lazy_dt(mtcars)
#'
#' # group_by() doesn't order by group
#' mtcars2 %>% group_by(cyl) %>% summarise(mpg = mean(mpg))
#'
#' # key_by() does
#' mtcars2 %>% key_by(cyl) %>% summarise(mpg = mean(mpg))
key_by <- function(.data, ...) {
  stopifnot(is_step(.data))

  dots <- unname(ensyms(...))
  cols <- vapply(dots, as.character, character(1))

  out <- step_call(.data, "setkeyv", list(cols = cols), in_place = TRUE)
  step_group(out, cols)
}
