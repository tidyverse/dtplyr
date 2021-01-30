step_modify <- function(parent, fun, args) {
  new_step(
    parent,
    groups = parent$groups,
    arrange = parent$arrange,
    implicit_copy = TRUE,
    fun = fun,
    args = args,
    class = "dtplyr_step_modify"
  )
}

dt_call.dtplyr_step_modify <- function(x, needs_copy = x$needs_copy) {
  j <- call2(x$fun, quote(.SD), quote(.BY), !!!x$args)
  out <- call2("[", dt_call(x$parent, needs_copy), , j)

  add_grouping_param(out, x)
}

# dplyr methods -----------------------------------------------------------

#' Apply a function to each group
#'
#' These are methods for the dplyr [group_map()] and [group_modify()] generics.
#' They are both translated to `[.data.table`.
#'
#' @param .tbl A [lazy_dt()]
#' @param .f The name of a two argument function. The first argument is passed
#'   `.SD`,the data.table representing the current group; the second argument
#'   is passed `.BY`, a list giving the current values of the grouping
#'   variables. The function should return a list or data.table.
#' @param ... Additional arguments passed to `.f`
#' @param keep Not supported for [lazy_dt].
#' @returns `group_map()` applies `.f` to each group, returning a list.
#'   `group_modify()` replaces each group with the results of `.f`, returning a
#'   modified [lazy_dt()].
#' @importFrom dplyr group_modify
#' @export
#' @examples
#' library(dplyr)
#'
#' dt <- lazy_dt(mtcars)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   group_modify(head, n = 2L)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   group_map(head, n = 2L)
group_modify.dtplyr_step <- function(.tbl, .f, ..., keep = FALSE) {
  if (!missing(keep)) {
    abort("`keep` is not supported for lazy data tables")
  }

  .f <- ensym(.f)
  args <- enquos(...)

  step_modify(.tbl, fun = .f, args = args)
}

#' @importFrom dplyr group_map
#' @rdname group_modify.dtplyr_step
#' @export
group_map.dtplyr_step <- function(.tbl, .f, ..., keep = FALSE) {
  .f <- as_function(.f, caller_env())

  dt <- as.data.table(.tbl)
  dt[, list(list(.f(.SD, .BY, ...))), by = eval(.tbl$groups)]$V1
}
