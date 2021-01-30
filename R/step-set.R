step_set <- function(x, y, style) {
  stopifnot(is_step(x))
  stopifnot(is_step(y))
  stopifnot(is.character(style))

  new_step(
    parent = x,
    parent2 = y,
    style = style,
    class = "dtplyr_step_set"
  )
}

#' @export
dt_sources.dtplyr_step_set <- function(x) {
  dt_sources.dtplyr_step_join(x)
}

#' @export
dt_call.dtplyr_step_set <- function(x, needs_copy = x$needs_copy) {
  lhs <- dt_call(x$parent, needs_copy)
  rhs <- dt_call(x$parent2)

  call <- switch(x$style,
    intersect = call2("fintersect", lhs, rhs),
    union = call2("funion", lhs, rhs),
    union_all = call2("funion", lhs, rhs, all = TRUE),
    setdiff = call2("fsetdiff", lhs, rhs),
  )

  call
}

# dplyr verbs -------------------------------------------------------------

#' Set operations
#'
#' These are methods for the dplyr generics [intersect()], [union()],
#' [union_all()], and [setdiff()]. They are translated to
#' [data.table::fintersect()], [data.table::funion()], and
#' [data.table::fsetdiff()].
#'
#' @importFrom dplyr intersect
#' @param x,y A pair of [lazy_dt()]s.
#' @param ... Ignored
#' @examples
#' dt1 <- lazy_dt(data.frame(x = 1:4))
#' dt2 <- lazy_dt(data.frame(x = c(2, 4, 6)))
#'
#' intersect(dt1, dt2)
#' union(dt1, dt2)
#' setdiff(dt1, dt2)
#'
# Exported onload
intersect.dtplyr_step <- function(x, y, ...) {
  step_set(x, y, style = "intersect")
}

#' @importFrom dplyr union
#' @rdname intersect.dtplyr_step
# Exported onload
union.dtplyr_step <- function(x, y, ...) {
  step_set(x, y, style = "union")
}

#' @importFrom dplyr union_all
#' @rdname intersect.dtplyr_step
#' @export
union_all.dtplyr_step <- function(x, y, ...) {
  step_set(x, y, style = "union_all")
}

#' @importFrom dplyr setdiff
#' @rdname intersect.dtplyr_step
# Exported onload
setdiff.dtplyr_step <- function(x, y, ...) {
  step_set(x, y, style = "setdiff")
}
