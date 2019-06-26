step_join <- function(x, y, on, style) {
  stopifnot(is_step(x))
  stopifnot(is_step(y))
  stopifnot(is.character(on))

  new_step(
    parent = x,
    implicit_copy = TRUE,
    parent2 = y,
    on = on,
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

  switch(x$style,
    inner = call2("merge", lhs, rhs, all = FALSE, by = by),
    full  = call2("merge", lhs, rhs, all = TRUE, by = by),
    left  = call2("[", lhs, rhs, on = on),
    semi = call2("[", lhs, call2("unique", call2("[", lhs, rhs, which = TRUE, nomatch = 0L, on = on))),
    anti  = call2("[", lhs, call2("!", rhs), on = on),
    abort("Invalid style")
  )
}

# dplyr verbs -------------------------------------------------------------

dtplyr_common_by <- function(by, x, y) {
  by <- dplyr::common_by(by, x, y)
  simplify_names(stats::setNames(by$x, by$y))
}

#' @importFrom dplyr left_join
#' @export
left_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  by <- dtplyr_common_by(by, x, y)
  # y <- auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "left")
}

#' @importFrom dplyr right_join
#' @export
right_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  by <- dtplyr_common_by(by, y, x)
  # y <- auto_copy(x, y, copy = copy)

  step_join(y, x, on = by, style = "left")
}

#' @importFrom dplyr inner_join
#' @export
inner_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  by <- dtplyr_common_by(by, x, y)
  # y <- auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "inner")
}

#' @importFrom dplyr full_join
#' @export
full_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  by <- dtplyr_common_by(by, x, y)
  # y <- auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "full")
}

#' @importFrom dplyr anti_join
#' @export
anti_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE) {
  by <- dtplyr_common_by(by, x, y)
  # y <- auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "anti")
}

#' @importFrom dplyr semi_join
#' @export
semi_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE) {
  by <- dtplyr_common_by(by, x, y)
  # y <- auto_copy(x, y, copy = copy)

  step_join(x, y, on = by, style = "semi")
}
