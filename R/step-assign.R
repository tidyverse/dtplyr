step_locals <- function(parent, locals, name) {
  stopifnot(is_step(parent))
  stopifnot(is.list(locals))
  stopifnot(is_string(name))

  new_step(
    parent = parent,
    locals = utils::modifyList(parent$locals, locals),
    implicit_copy = TRUE,
    needs_copy = FALSE,
    name = name,
    class = "dtplyr_step_assign",
  )
}

#' @export
dt_call.dtplyr_step_assign <- function(x, needs_copy = FALSE) {
  sym(x$name)
}
