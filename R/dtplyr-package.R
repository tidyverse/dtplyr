#' @import rlang
#' @importFrom data.table data.table as.data.table .SD copy is.data.table
#' @importFrom lifecycle deprecated
#' @keywords internal
"_PACKAGE"

#' @export
.datatable.aware <- TRUE

globalVariables(c(".N", ".BY", "desc"))
