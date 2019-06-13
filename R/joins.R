#' Join data table tbls.
#'
#' See \code{\link{join}} for a description of the general purpose of the
#' functions.
#'
#' @inheritParams dplyr::join
#' @param x,y tbls to join
#' @param ... For `inner_join()`, `left_join()`, `right_join()` and
#'   `full_join()` passed on to data.table [merge()] method. For
#'   `semi_join()` and `anti_join()` included only for compatibility with
#'   generic and must be empty.
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' if (require("Lahman")) {
#' batting_dt <- tbl_dt(Batting)
#' person_dt <- tbl_dt(Master)
#'
#' # Inner join: match batting and person data
#' inner_join(batting_dt, person_dt)
#'
#' # Left join: keep batting data even if person missing
#' left_join(batting_dt, person_dt)
#'
#' # Semi-join: find batting data for top 4 teams, 2010:2012
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"),
#'   yearID = 2010:2012)
#' top4 <- semi_join(batting_dt, grid, copy = TRUE)
#'
#' # Anti-join: find batting data with out player data
#' anti_join(batting_dt, person_dt)
#' }
#' @name join.tbl_dt
NULL

join_using_merge <- function(x, y, ..., by, copy, suffix,
                             all.x = FALSE, all.y = FALSE){
  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  out <- merge(
    x, y,
    by.x = by$x, by.y = by$y,
    all.x = all.x, all.y = all.y,
    suffixes = suffix,
    allow.cartesian = TRUE,
    ...
  )
  grouped_dt(out, groups(x))
}

#' @rdname join.tbl_dt
inner_join.data.table <- function(x, y, ..., by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y")){
  join_using_merge(x, y, by = by, copy = copy, suffix = suffix, ...)
}

#' @rdname join.tbl_dt
left_join.data.table <- function(x, y, ..., by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y")){
  join_using_merge(x, y, by = by, copy = copy, suffix = suffix, all.x = TRUE, ...)
}

#' @rdname join.tbl_dt
right_join.data.table <- function(x, y, ..., by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y")){
  join_using_merge(x, y, by = by, copy = copy, suffix = suffix, all.y = TRUE, ...)
}

#' @rdname join.tbl_dt
full_join.data.table <- function(x, y, ..., by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y")){
  join_using_merge(x, y,
    by = by,
    copy = copy,
    suffix = suffix,
    all.x = TRUE,
    all.y = TRUE,
    ...
  )
}

#' @rdname join.tbl_dt
semi_join.data.table <- function(x, y, by = NULL, copy = FALSE, ...) {
  ellipsis::check_dots_empty()

  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  on <- set_names(by$y, by$x)
  y_trimmed <- y[, by$y, with = FALSE]
  w <- x[y_trimmed, which = TRUE, on = on, nomatch = 0L]
  out <- x[sort(unique(w))]
  grouped_dt(out, groups(x))
}

#' @rdname join.tbl_dt
anti_join.data.table <- function(x, y, by = NULL, copy = FALSE, ...) {
  ellipsis::check_dots_empty()

  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  on <- set_names(by$y, by$x)
  y_trimmed <- y[, by$y, with = FALSE]
  w <- x[!y_trimmed, which = TRUE, on = on]
  out <- x[sort(unique(w))]
  grouped_dt(out, groups(x))
}
