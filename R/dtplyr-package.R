#' @import rlang
#' @importFrom data.table data.table as.data.table is.data.table
#' @importFrom lifecycle deprecated
#' @keywords internal
"_PACKAGE"

#' @export
.datatable.aware <- TRUE

globalVariables(c(".SD", ".N", ".BY", ".I", "desc"))
