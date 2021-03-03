step_join <- function(x, y, on, style, suffix = c(".x", ".y")) {
  stopifnot(is_step(x))
  stopifnot(is_step(y))
  stopifnot(is.character(on))

  style <- match.arg(style, c("inner", "full", "left", "semi", "anti"))
  if (style %in% c("semi", "anti")) {
    vars <- x$vars
  } else {
    vars <- join_vars(x$vars, y$vars, on, suffix)
  }

  new_step(
    parent = x,
    implicit_copy = TRUE,
    parent2 = y,
    vars = vars,
    on = on,
    suffix = suffix,
    style = style,
    locals = utils::modifyList(x$locals, y$locals),
    class = "dtplyr_step_join"
  )
}

#' @export
dt_sources.dtplyr_step_join <- function(x) {
  # TODO: need to throw error if same name refers to different tables.
  utils::modifyList(dt_sources(x$parent), dt_sources(x$parent2))
}

#' @export
dt_call.dtplyr_step_join <- function(x, needs_copy = x$needs_copy) {
  lhs <- dt_call(x$parent, needs_copy)
  rhs <- dt_call(x$parent2)
  on <- call2(".", !!!syms(x$on))

  by.x <- as.character(x$on)
  by.y <- ifelse(names(x$on) == "", by.x, names(x$on))

  call <- switch(x$style,
    inner = call2("merge", lhs, rhs, all = FALSE, by.x = by.x, by.y = by.y, allow.cartesian = TRUE),
    full  = call2("merge", lhs, rhs, all = TRUE, by.x = by.x, by.y = by.y, allow.cartesian = TRUE),
    left  = call2("merge", lhs, rhs, all.x = TRUE, all.y = FALSE, by.x = by.x, by.y = by.y, allow.cartesian = TRUE),
    semi = call2("[", lhs, call2("unique", call2("[", lhs, rhs, which = TRUE, nomatch = NULL, on = on))),
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

#' Join data tables
#'
#' These are methods for the dplyr generics [left_join()], [right_join()],
#' [inner_join()], [full_join()], [anti_join()], and [semi_join()]. The
#' mutating joins (left, right, inner, and full) are translated to
#' [data.table::merge.data.table()], except for the special cases where it's
#' possible to translate to `[.data.table`. Semi- and anti-joins have no
#' direct data.table equivalent.
#'
#' @param x,y A pair of [lazy_dt()]s.
#' @inheritParams dplyr::left_join
#' @importFrom dplyr left_join
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' band_dt <- lazy_dt(dplyr::band_members)
#' instrument_dt <- lazy_dt(dplyr::band_instruments)
#'
#' band_dt %>% left_join(instrument_dt)
#' band_dt %>% right_join(instrument_dt)
#' band_dt %>% inner_join(instrument_dt)
#' band_dt %>% full_join(instrument_dt)
#'
#' band_dt %>% semi_join(instrument_dt)
#' band_dt %>% anti_join(instrument_dt)
left_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  y <- dtplyr_auto_copy(x, y, copy = copy)
  by <- dtplyr_common_by(by, x, y)

  if (join_is_simple(x$vars, y$vars, by)) {
    step_subset_on(y, x, i = y, on = by)
  } else {
    step_join(x, y, on = by, style = "left", suffix = suffix)
  }
}

#' @export
left_join.data.table <- function(x, y, ...) {
  x <- lazy_dt(x)
  left_join(x, y, ...)
}

#' @importFrom dplyr right_join
#' @export
right_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  y <- dtplyr_auto_copy(x, y, copy = copy)
  by <- dtplyr_common_by(by, y, x)

  if (join_is_simple(x$vars, y$vars, by)) {
    step_subset_on(x, y, i = y, on = by)
  } else {
    step_join(y, x, on = by, style = "left", suffix = suffix)
  }
}

#' @export
right_join.data.table <- function(x, y, ...) {
  x <- lazy_dt(x)
  right_join(x, y, ...)
}


step_subset_on <- function(x, y, i, on) {
  step_subset(x,
    vars = union(x$vars, y$vars),
    i = y,
    on = on,
    locals = utils::modifyList(x$locals, y$locals)
  )
}

#' @importFrom dplyr inner_join
#' @export
inner_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  y <- dtplyr_auto_copy(x, y, copy = copy)
  by <- dtplyr_common_by(by, x, y)

  step_join(x, y, on = by, style = "inner", suffix = suffix)
}

#' @export
inner_join.data.table <- function(x, y, ...) {
  x <- lazy_dt(x)
  inner_join(x, y, ...)
}

#' @importFrom dplyr full_join
#' @export
full_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y")) {
  y <- dtplyr_auto_copy(x, y, copy = copy)
  by <- dtplyr_common_by(by, x, y)

  step_join(x, y, on = by, style = "full", suffix = suffix)
}

#' @export
full_join.data.table <- function(x, y, ...) {
  x <- lazy_dt(x)
  full_join(x, y, ...)
}

#' @importFrom dplyr anti_join
#' @export
anti_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE) {
  y <- dtplyr_auto_copy(x, y, copy = copy)
  by <- dtplyr_common_by(by, x, y)

  step_join(x, y, on = by, style = "anti")
}

#' @export
anti_join.data.table <- function(x, y, ...) {
  x <- lazy_dt(x)
  anti_join(x, y, ...)
}

#' @importFrom dplyr semi_join
#' @export
semi_join.dtplyr_step <- function(x, y, ..., by = NULL, copy = FALSE) {
  y <- dtplyr_auto_copy(x, y, copy = copy)
  by <- dtplyr_common_by(by, x, y)

  step_join(x, y, on = by, style = "semi")
}

#' @export
semi_join.data.table <- function(x, y, ...) {
  x <- lazy_dt(x)
  semi_join(x, y, ...)
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

join_is_simple <- function(x, y, by) {
  if (is_named(by)) {
    return(FALSE)
  }

  common_vars <- setdiff(intersect(x, y), by)
  length(common_vars) == 0
}

join_vars <- function(x, y, on, suffixes) {
  y <- setdiff(y, if (is_named(on)) names(on) else on)
  vars <- union(x, y)

  both <- intersect(x, y)
  if (length(both) > 0) {
    vars <- c(setdiff(vars, both), paste0(both, suffixes[[1]]), paste0(both, suffixes[[2]]))
  }

  vars
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
#' @export
tbl_vars.foo <- function(x) "x"
#' @export
as.data.frame.foo <- function(x, ...) data.frame(x = 1:10)
