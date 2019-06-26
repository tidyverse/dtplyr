step_join <- function(x, y, on, style, suffix = c(".x", ".y")) {
  stopifnot(is_step(x))
  stopifnot(is_step(y))
  stopifnot(is.character(on))

  new_step(
    parent = x,
    implicit_copy = TRUE,
    parent2 = y,
    on = on,
    suffix = suffix,
    style = style,
    class = "dtplyr_step_join"
  )
}

dt_sources.dtplyr_step_join <- function(x) {
  # TODO: need to throw error if same name refers to different tables.
  utils::modifyList(dt_sources(x$parent), dt_sources(x$parent2))
}

dt_call.dtplyr_step_join <- function(x, needs_copy = x$needs_copy) {
  lhs <- dt_call(x$parent, needs_copy)
  rhs <- dt_call(x$parent2)
  on <- call2(".", !!!syms(x$on))
  by <- as.character(x$on)

  call <- switch(x$style,
    inner = call2("merge", lhs, rhs, all = FALSE, by = by),
    full  = call2("merge", lhs, rhs, all = TRUE, by = by),
    left  = call2("merge", lhs, rhs, all.x = TRUE, all.y = FALSE, by = by),
    semi = call2("[", lhs, call2("unique", call2("[", lhs, rhs, which = TRUE, nomatch = 0L, on = on))),
    anti  = call2("[", lhs, call2("!", rhs), on = on),
    abort("Invalid style")
  )

  # Hack on suffix if not the default
  if (is_call(call, "merge") && !identical(x$suffix, c(".x", ".y"))) {
    call$suffixes <- x$suffix
  }

  call
}

# dplyr verbs -------------------------------------------------------------

#' @importFrom dplyr left_join
#' @export
left_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  by <- dtplyr_common_by(by, x, y)
  y <- dtplyr_auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "left", suffix = suffix)
}

#' @importFrom dplyr right_join
#' @export
right_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  by <- dtplyr_common_by(by, y, x)
  y <- dtplyr_auto_copy(x, y, copy = copy)

  step_join(y, x, on = by, style = "left", suffix = suffix)
}

#' @importFrom dplyr inner_join
#' @export
inner_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  by <- dtplyr_common_by(by, x, y)
  y <- dtplyr_auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "inner", suffix = suffix)
}

#' @importFrom dplyr full_join
#' @export
full_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  by <- dtplyr_common_by(by, x, y)
  y <- dtplyr_auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "full", suffix = suffix)
}

#' @importFrom dplyr anti_join
#' @export
anti_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE) {
  by <- dtplyr_common_by(by, x, y)
  y <- dtplyr_auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "anti")
}

#' @importFrom dplyr semi_join
#' @export
semi_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE) {
  by <- dtplyr_common_by(by, x, y)
  y <- dtplyr_auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "semi")
}

# helpers -----------------------------------------------------------------

dtplyr_common_by <- function(by, x, y) {
  by <- dplyr::common_by(by, x, y)
  simplify_names(stats::setNames(by$x, by$y))
}

dtplyr_auto_copy <- function(x, y, copy = copy) {
  if (is_step(y)) {
    y
  } else if (is.data.frame(y)) { # includes data tables
    lazy_dt(y)
  } else {
    dplyr::auto_copy(x, y, copy = copy)
  }
}

#' @importFrom dplyr same_src
#' @export
same_src.dtplyr_step <- function(x, y) {
  is_step(y)
}

#' @importFrom dplyr auto_copy
#' @export
auto_copy.dtplyr_step <- function(x, y, copy = FALSE, ...) {
  lazy_dt(as.data.frame(y))
}

# Needed to test auto_copy
tbl_vars.foo <- function(x) "x"
as.data.frame.foo <- function(x) data.frame(x = 1:10)
