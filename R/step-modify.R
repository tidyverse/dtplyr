step_modify <- function(parent, fun, args) {
  new_step(
    parent,
    groups = parent$groups,
    keyby = parent$keyby,
    implicit_copy = TRUE,
    fun = fun,
    args = args,
    class = "dtplyr_step_modify"
  )
}

dt_call.dtplyr_step_modify <- function(x, needs_copy = x$needs_copy) {
  j <- call2(x$fun, quote(.SD), quote(.BY), !!!x$args)
  out <- call2("[", dt_call(x$parent, needs_copy), , j)

  add_grouping_parameter(out, x$groups, x$keyby)
}

# dplyr methods -----------------------------------------------------------

#' Modify a lazy_dt in place
#'
#' `group_modify()` applies `.f` to each group, returning a modified
#' [lazy_dt()]. This function is a little less flexible than the data.frame
#' method due to the constraints of the code generation that dtplyr uses.
#'
#' @param .tbl A [lazy_dt]
#' @param .f The name of a two argument function. The first argument is passed
#'   `.SD`,the data.table representing the current group; the second argument
#'   is passed `.BY`, a list giving the current values of the grouping
#'   variables. The function should return a list or data.table.
#' @param ... Additional arguments passed to `.f`
#' @param keep Not supported for [lazy_dt].
#' @importFrom dplyr group_modify
#' @export
#' @examples
#' library(dplyr)
#'
#' mtcars %>%
#'   lazy_dt() %>%
#'   group_by(cyl) %>%
#'   group_modify(head, n = 2L)
group_modify.dtplyr_step <- function(.tbl, .f, ..., keep = FALSE) {
  if (!missing(keep)) {
    abort("`keep` is not supported for lazy data tables")
  }

  .f <- ensym(.f)
  args <- enquos(...)

  step_modify(.tbl, fun = .f, args = args)
}
