#' @rawNamespace
#' if (utils::packageVersion("dplyr") > "0.5.0") {
#'   importFrom(dplyr,do)
#'   S3method(do,data.table)
#' }
#' @importFrom dplyr do
do.data.table <- function(.data, ...) {
  out <- do(as.data.frame(.data), !!! quos(...))
  data.table::as.data.table(out)
}
do.tbl_dt <- function(.data, ...) {
  out <- do(as.data.frame(.data), !!! quos(...))
  tbl_dt(out)
}
