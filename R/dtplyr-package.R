#' @import rlang
#' @importFrom data.table data.table as.data.table is.data.table
#' @importFrom lifecycle deprecated
#' @importFrom glue glue
#' @keywords internal
"_PACKAGE"

#' dtplyr is data.table aware
#'
#' @keywords internal
#' @export
.datatable.aware <- TRUE

globalVariables(c(".SD", ".N", ".BY", ".I", "desc"))
