#' Join data table tbls.
#'
#' See \code{\link{join}} for a description of the general purpose of the
#' functions.
#'
#' @inheritParams dplyr::join
#' @param x,y tbls to join
#' @param ... Included for compatibility with generic; otherwise ignored.
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

#' @rdname join.tbl_dt
inner_join.data.table <- function(x, y, by = NULL, copy = FALSE, ...){
  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  out <- merge(x, y, by.x = by$x, by.y = by$y, all = FALSE, allow.cartesian = TRUE)
  grouped_dt(out, groups(x)) 
}

#' @rdname join.tbl_dt
left_join.data.table <- function(x, y, by = NULL, copy = FALSE, ...){
  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  out <- merge(x, y, by.x = by$x, by.y = by$y, all.x = TRUE, allow.cartesian = TRUE)
  grouped_dt(out, groups(x)) 
}

#' @rdname join.tbl_dt
right_join.data.table <- function(x, y, by = NULL, copy = FALSE, ...){
  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  out <- merge(x, y, by.x = by$x, by.y = by$y, all.y = TRUE, allow.cartesian = TRUE)
  grouped_dt(out, groups(x)) 
}

#' @rdname join.tbl_dt
semi_join.data.table <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  by_x <- by$x
  by_y <- by$y
  y_filter <- y[, by_y, with = FALSE]
  names(y_filter) <- by_x
  w <- x[y_filter, which = TRUE, on = by_x, nomatch = 0L]
  out <- x[sort(unique(w))]
  grouped_dt(out, groups(x))
}

#' @rdname join.tbl_dt
anti_join.data.table <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  by_x <- by$x
  by_y <- by$y
  y_filter <- y[, by_y, with = FALSE]
  names(y_filter) <- by_x
  w <- x[!y_filter, which = TRUE, on = by_x]
  out <- x[sort(unique(w))]
  grouped_dt(out, groups(x))
}

#' @rdname join.tbl_dt
full_join.data.table <- function(x, y, by = NULL, copy = FALSE, ...){
  by <- dplyr::common_by(by, x, y)
  y <- dplyr::auto_copy(x, y, copy = copy)
  out <- merge(x, y, by.x = by$x, by.y = by$y, all = TRUE, allow.cartesian = TRUE)
  grouped_dt(out, groups(x)) 
}

