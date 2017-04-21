#' @rawNamespace
#' if (utils::packageVersion("dplyr") > "0.5.0") {
#'   importFrom(dplyr,do)
#'   S3method(do,data.table)
#' }
do.data.table <- function(.data, ...) {
  do_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @importFrom dplyr do_
do_.data.table <- function(.data, ..., .dots) {
  out <- do_(as.data.frame(.data), ..., .dots = .dots)
  data.table::as.data.table(out)
}
do_.tbl_dt <- function(.data, ..., .dots) {
  out <- do_(as.data.frame(.data), ..., .dots = .dots)
  tbl_dt(out)
}
