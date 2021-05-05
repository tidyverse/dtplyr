step_join <- function(x, y, on, style, suffix = c(".x", ".y")) {
  stopifnot(is_step(x))
  stopifnot(is_step(y))
  stopifnot(is.character(on))
  style <- match.arg(style, c("inner", "full", "right", "left", "semi", "anti"))

  on <- dplyr::common_by(on, x, y)

  if (style %in% c("semi", "anti")) {
    vars <- x$vars
  } else {
    x_sim <- simulate_vars(x)
    y_sim <- simulate_vars(y)
    vars <- colnames(left_join(x_sim, y_sim, by = purrr::set_names(on$y, on$x)))
  }

  on_yx <- list(x = on$y, y = on$x)

  out <- new_step(
    parent = x,
    implicit_copy = TRUE,
    parent2 = y,
    vars = vars,
    on = if (style %in% c("left", "full")) on else on_yx,
    style = style,
    locals = utils::modifyList(x$locals, y$locals),
    class = "dtplyr_step_join"
  )

  if (style %in% c("anti", "semi")) {
    return(out)
  }

  if (style == "left") {
    colorder <- join_colorder_dt_left(x$vars, y$vars, on$x, on$y)

    # need to swap `x` and `y` as the data.table left join is `y[x, on]`
    vars_out_dt <- join_vars_dt(y$vars, x$vars, on_y = on$x)
    vars_out_dt <- vars_out_dt[colorder]

    if (any(colorder != seq_along(colorder))) {
      out <- step_colorder(out, vars_out_dt)
    }
  } else if (style %in% c("right", "inner")) {
    vars_out_dt <- join_vars_dt(x$vars, y$vars, on$y)
  } else if (style == "full") {
    vars_out_dt <- merge_vars(x$vars, y$vars, on$x, on$y, suffix)
  } else {
    abort("unexpected join style")
  }

  same <- vars_out_dt == vars
  if (!all(same)) {
    out <- step_call(
      out,
      "setnames",
      args = list(vars_out_dt[!same], vars[!same]),
      vars = vars,
      in_place = TRUE
    )
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
  on2 <- simplify_names(stats::setNames(x$on$x, x$on$y))

  on <- call2(".", !!!syms(on2))

  switch(x$style,
    left = call2("[", rhs, lhs, on = on, allow.cartesian = TRUE),
    full = call2("merge", lhs, rhs, all = TRUE, by.x = x$on$x, by.y = x$on$y, allow.cartesian = TRUE),
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
  step_join(x, y, on = by, style = "semi")
}

#' @export
semi_join.data.table <- function(x, y, ...) {
  x <- lazy_dt(x)
  semi_join(x, y, ...)
}

# helpers -----------------------------------------------------------------

dtplyr_auto_copy <- function(x, y, copy = copy) {
  if (is_step(y)) {
    y
  } else if (is.data.frame(y)) { # includes data tables
    lazy_dt(y)
  } else {
    dplyr::auto_copy(x, y, copy = copy)
  }
}

add_suffixes <- function (x, y, suffix) {
  if (identical(suffix, "")) {
    return(x)
  }
  out <- rep_along(x, na_chr)
  for (i in seq_along(x)) {
    nm <- x[[i]]
    while (nm %in% y || nm %in% out[seq_len(i - 1)]) {
      nm <- paste0(nm, suffix)
    }
    out[[i]] <- nm
  }
  out
}

#' @noRd
#' column names as generated in `x[y, on = on]`
join_vars_dt <- function(x, y, on_y) {
  # `y` variables used for joining are not included again
  y_out <- setdiff(y, on_y)
  # remaining `y` columns that are also in `x` get _prefixed_ by "i."
  y_out[y_out %in% x] <- paste0("i.", y_out[y_out %in% x])
  c(x, y_out)
}

join_colorder_dt_left <- function(x, y, on_x, on_y) {
  #' data.table: y-vars, x-vars - on_x
  #' dplyr: x-vars, y-vars - on_y

  x_out_dt <- setdiff(x, on_x)
  x_loc <- vctrs::vec_match(x, x_out_dt) + length(y)
  x_loc[is.na(x_loc)] <- vctrs::vec_match(on_y, y)

  y_out_dt <- setdiff(y, on_y)
  y_loc <- vctrs::vec_match(y_out_dt, y)

  c(x_loc, y_loc)
}

#' column names as generated by `data.table::merge(x, y)`
merge_vars <- function(x, y, on_x, on_y, suffix = c(".x", ".y")) {
  x <- setdiff(x, on_x)
  y <- setdiff(y, on_y)

  x_out <- add_suffixes(x, y, suffix[[1]])
  y_out <- add_suffixes(y, x, suffix[[2]])

  c(on_x, x_out, y_out)
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
