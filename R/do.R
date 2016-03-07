#' @importFrom dplyr do_
#' @export
do_.data.table <- function(.data, ..., .dots) {
  out <- do_(as.data.frame(.data), ..., .dots = .dots)
  data.table::as.data.table(out)
}

#' @export
do_.tbl_dt <- function(.data, ..., .dots) {
  out <- do_(as.data.frame(.data), ..., .dots = .dots)
  tbl_dt(out)
}
