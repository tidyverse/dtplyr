step_join <- function(x, y, on, style, suffix = c(".x", ".y")) {
  stopifnot(is_step(x))
  stopifnot(is_step(y))
  stopifnot(is.null(on) || is.character(on))
  style <- match.arg(style, c("inner", "full", "right", "left", "semi", "anti"))

  if (is_character(on, 0)) {
    return(cross_join(x, y))
  }

  on <- dplyr::common_by(on, x, y)

  vars_out_dt <- dt_join_vars(x$vars, y$vars, on$x, on$y, suffix = suffix, style = style)
  colorder <- dt_join_colorder(x$vars, y$vars, on$x, on$y, style)

  # TODO suppress warning in merge
  # "column names ... are duplicated in the result
  out <- new_step(
    parent = if (style == "left") y else x,
    implicit_copy = TRUE,
    parent2 = if (style == "left") x else y,
    vars = vars_out_dt,
    on = if (style %in% c("left", "full")) on else list(x = on$y, y = on$x),
    style = style,
    locals = utils::modifyList(x$locals, y$locals),
    class = "dtplyr_step_join"
  )

  if (style %in% c("anti", "semi")) {
    return(out)
  }

  out <- step_colorder(out, colorder)

  x_sim <- simulate_vars(x)
  y_sim <- simulate_vars(y)
  vars <- dplyr_join_vars(x_sim, y_sim, on$x, on$y, suffix = suffix)

  if (any(duplicated(vars_out_dt))) {
    step_setnames(out, colorder, vars, in_place = FALSE)
  } else {
    step_setnames(out, vars_out_dt[colorder], vars, in_place = FALSE)
  }
}

cross_join <- function(x, y) {
  xy <- left_join(
    mutate(x, .cross_join_col = 1),
    mutate(y, .cross_join_col = 1),
    by = ".cross_join_col"
  )

  # use custom select to produce way shorter query
  step_subset_j(
    xy,
    vars = setdiff(xy$vars, ".cross_join_col"),
    j = expr(!".cross_join_col")
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
  on2 <- simplify_names(stats::setNames(x$on$x, x$on$y))

  on <- call2(".", !!!syms(on2))

  switch(x$style,
    full = call2("merge", lhs, rhs, all = TRUE, by.x = x$on$x, by.y = x$on$y, allow.cartesian = TRUE),
    left = call2("[", lhs, rhs, on = on, allow.cartesian = TRUE),
    inner = call2("[", lhs, rhs, on = on, nomatch = NULL),
    right = call2("[", lhs, rhs, on = on, allow.cartesian = TRUE),
    anti = call2("[", lhs, call2("!", rhs), on = on),
    semi = call2("[", lhs, rhs, on = on, nomatch = NULL, mult = 'first')
  )
}

# dplyr verbs -------------------------------------------------------------

#' Join data tables
#'
#' These are methods for the dplyr generics [left_join()], [right_join()],
#' [inner_join()], [full_join()], [anti_join()], and [semi_join()]. Left, right,
#' inner, and anti join are translated to the `[.data.table` equivalent,
#' full joins to [data.table::merge.data.table()].
#' Left, right, and full joins are in some cases followed by calls to
#' [data.table::setcolorder()] and [data.table::setnames()] to ensure that column
#' order and names match dplyr conventions.
#' Semi-joins don't have a direct data.table equivalent.
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
  x[x %in% y] <- paste0(x[x %in% y], suffix)
  x
}

dplyr_join_vars <- function(x, y, on_x, on_y, suffix) {
  colnames(left_join(x, y, by = stats::setNames(on_y, on_x), suffix = suffix))
}

dt_join_vars <- function(x, y, on_x, on_y, suffix, style) {
  style <- match.arg(style, c("inner", "full", "right", "left", "semi", "anti"))

  if (style == "left") {
    # need to swap `x` and `y` as the data.table left join is `y[x, on]`
    subset_join_vars(y, x, on_y = on_x)
  } else if (style %in% c("right", "inner")) {
    subset_join_vars(x, y, on_y)
  } else if (style == "full") {
    merge_vars(x, y, on_x, on_y, suffix)
  } else {
    x
  }
}

# column names as generated in `x[y, on = on]`
subset_join_vars <- function(x, y, on_y) {
  # `y` variables used for joining are not included again
  y_out <- setdiff(y, on_y)
  # remaining `y` columns that are also in `x` get _prefixed_ by "i."
  y_out[y_out %in% x] <- paste0("i.", y_out[y_out %in% x])
  out_names <- c(x, y_out)

  add_dt_suffix(out_names)
}

add_dt_suffix <- function(x) {
  for (i in seq_along(x)) {
    j <- 1
    nm <- x[[i]]
    first_occurrence <- !nm %in% x[seq(0, i - 1)]
    if (!first_occurrence) {
      while (nm %in% x[-i]) {
        nm <- paste0(x[[i]], ".", j)
        j <- j + 1
      }
    }
    x[[i]] <- nm
  }
  x
}

# column names as generated by `merge(x, y, by.x = on_x, by.y = on_y)`
merge_vars <- function(x, y, on_x, on_y, suffix = c(".x", ".y")) {
  x <- setdiff(x, on_x)
  y <- setdiff(y, on_y)

  x_out <- add_suffixes(x, y, suffix[[1]])
  y_out <- add_suffixes(y, x, suffix[[2]])

  c(on_x, x_out, y_out)
}

dt_join_colorder <- function(x, y, on_x, on_y, style) {
  style <- match.arg(style, c("inner", "full", "right", "left", "semi", "anti"))

  if (style == "left") {
    subset_left_join_colorder(x, y, on_x, on_y)
  } else if (style == "full") {
    merge_join_colorder(x, y, on_x, on_y)
  } else {
    seq(length(x) + length(y) - length(on_x))
  }
}

#' column order of data.table left join `y[x]` compared to `left_join(y, x)`
#' @noRd
subset_left_join_colorder <- function(x, y, on_x, on_y) {
  # variable order
  # y[x, on]: y-vars, x-vars - on_x
  # left_join(x, y, on): x-vars, y-vars - on_y

  x_out_dt <- setdiff(x, on_x)
  x_loc <- vctrs::vec_match(x, x_out_dt) + length(y)
  x_loc[is.na(x_loc)] <- vctrs::vec_match(on_y, y)

  y_out_dt <- setdiff(y, on_y)
  y_loc <- vctrs::vec_match(y_out_dt, y)

  c(x_loc, y_loc)
}

merge_join_colorder <- function(x, y, on_x, on_y) {
  # variable order
  # merge(x, y, on_x, on_y): on_x, x-vars - on_x, y-vars - on_y
  # full_join(x, y, on): x-vars, y-vars - on_y

  x_out_dt <- setdiff(x, on_x)
  x_loc <- vctrs::vec_match(x, x_out_dt) + length(on_x)
  x_loc[is.na(x_loc)] <- vctrs::vec_match(x[is.na(x_loc)], on_x)

  n_x <- length(x)
  n_y_out <- length(y) - length(on_x)

  c(x_loc, n_x + seq2(1, n_y_out))
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
