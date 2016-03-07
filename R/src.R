#' A local data table source.
#'
#' This is mainly useful for testing, since makes it possible to refer to
#' local and remote tables using exactly the same syntax.
#'
#' @rdname src_local
#' @export
#' @param pkg,env Either the name of a package or an environment object in
#'   which to look for objects.
src_dt <- function(pkg = NULL, env = NULL) {
  dplyr::src_local("tbl_dt", pkg, env)
}
