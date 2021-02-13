#' @import rlang
#' @importFrom data.table data.table as.data.table is.data.table
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom vctrs vec_as_names
#' @keywords internal
"_PACKAGE"

#' @export
.datatable.aware <- TRUE

globalVariables(c(".SD", ".N", ".BY", ".I", "desc"))
