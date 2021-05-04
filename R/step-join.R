step_join <- function(x, y, on, style, suffix = c(".x", ".y")) {
  stopifnot(is_step(x))
  stopifnot(is_step(y))
  stopifnot(is.character(on))
  style <- match.arg(style, c("inner", "full", "right", "left", "semi", "anti"))

  if (style %in% c("semi", "anti")) {
    vars <- x$vars
  } else {
    vars <- join_vars(x$vars, y$vars, on, suffix)
  }

  on_org <- set_names(names2(on), on)
  on_org[on_org == ""] <- on[on_org == ""]
  on_org <- simplify_names(on_org)

  out <- new_step(
    parent = x,
    implicit_copy = TRUE,
    parent2 = y,
    vars = vars,
    on = if (style %in% c("left")) on else on_org,
    style = style,
    locals = utils::modifyList(x$locals, y$locals),
    class = "dtplyr_step_join"
  )

  if (style %in% c("anti", "semi")) {
    return(out)
  }

  if (style == "left") {
    vars_out_dt <- join_vars_dt(y$vars, x$vars, on)
    vars_out_dt2 <- join_vars_dt_dplyr_left(x$vars, y$vars, on_org, suffix)
  } else if (style %in% c("right", "inner")) {
    vars_out_dt <- join_vars_dt(x$vars, y$vars, on_org)
    vars_out_dt2 <- join_vars_dt_dplyr_right(x$vars, y$vars, on_org, suffix)
  } else {
    vars_out_dt <- merge_vars(x$vars, y$vars, on_org, suffix)
    vars_out_dt2 <- vars_out_dt
  }

  same <- vars_out_dt == vars_out_dt2

  if (!all(same)) {
    out <- step_call(
      out,
      "setnames",
      args = list(vars_out_dt[!same], vars_out_dt2[!same]),
      vars = vars_out_dt2,
      in_place = TRUE
    )
  }

  # TODO do not use unexported dplyr function
  vars <- dplyr:::join_cols(x$vars, y$vars, by = on_org, suffix = suffix)
  colorder <- c(names(vars$x$out), names(vars$y$out))

  if (any(colorder != vars_out_dt2)) {
    out <- step_colorder(out, colorder)
  }

  out

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

  by.x <- names2(x$on)
  by.x[by.x == ""] <- x$on[by.x == ""]
  by.y <- unname(x$on)

  switch(x$style,
    left = call2("[", rhs, lhs, on = on, allow.cartesian = TRUE),
    full = call2("merge", lhs, rhs, all = TRUE, by.x = by.x, by.y = by.y, allow.cartesian = TRUE),
    right = call2("[", lhs, rhs, on = on, allow.cartesian = TRUE),
    anti = call2("[", lhs, call2("!", rhs), on = on),
    semi = call2("[", lhs, call2("unique", call2("[", lhs, rhs, which = TRUE, nomatch = NULL, on = on))),
    inner = call2("[", lhs, rhs, on = on, nomatch = NULL)
  )
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

  step_join(x, y, by, style = "left", suffix = suffix)
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
  by <- dtplyr_common_by(by, x, y)

  step_join(x, y, by, style = "right", suffix = suffix)
}

#' @export
right_join.data.table <- function(x, y, ...) {
  x <- lazy_dt(x)
  right_join(x, y, ...)
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

join_vars <- function(x, y, on, suffixes) {
  on_y <- names2(on)
  on_y[on_y == ""] <- on[on_y == ""]

  y <- setdiff(y, on_y)
  vars <- union(x, y)

  both <- intersect(x, y)
  if (length(both) > 0) {
    vars <- c(setdiff(vars, both), paste0(both, suffixes[[1]]), paste0(both, suffixes[[2]]))
  }

  vars
}

#' @noRd
#' column names as generated in `x[y, on = by]`
#' variables produced by a datatable join:
#' `x[y, on = on]`
join_vars_dt <- function(x, y, on) {
  # join variables are already included in `x`
  y_out <- setdiff(y, on)
  # variables that are in `x` and `y` and not joined by get prefixed by "i."
  y_out[y_out %in% x] <- paste0("i.", y_out[y_out %in% x])
  c(x, y_out)
}

#' @noRd
#' variables as they should be named according to dplyr
join_vars_dt_dplyr_left <- function(x, y, by, suffix = c(".x", ".y")) {
  nms <- names2(by)
  nms[nms == ""] <- by[nms == ""]

  # rename y
  y_out <- purrr::set_names(y)
  idx <- vctrs::vec_match(by, y_out)
  y_out[by] <- nms[idx]

  x_out <- setdiff(x, nms)
  xy_out <- add_suffix(x_out, y_out, suffix)

  c(unname(xy_out$y), xy_out$x)
}

join_vars_dt_dplyr_right <- function(x, y, by, suffix = c(".x", ".y")) {
  y_out <- setdiff(y, by)
  xy_out <- add_suffix(x, y_out, suffix)

  c(xy_out$x, unname(xy_out$y))
}

merge_vars <- function(x, y, by, suffix = c(".x", ".y")) {
  nms <- names2(by)
  nms[nms == ""] <- by[nms == ""]

  x_out <- setdiff(x, nms)
  y_out <- setdiff(y, by)

  xy_out <- add_suffix(x_out, y_out, suffix)

  c(nms, xy_out$x, xy_out$y)
}

add_suffix <- function(x, y, suffix) {
  both <- intersect(x, y)
  if (length(both) > 0) {
    x[x %in% both] <- paste0(x[x %in% both], suffix[[1]])
    y[y %in% both] <- paste0(y[y %in% both], suffix[[2]])
  }

  list(x = x, y = y)
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
