new_step <- function(parent,
                     vars = parent$vars,
                     groups = parent$groups,
                     env = caller_env(),
                     ...,
                     class = character()) {

  stopifnot(is.data.table(parent) || is_step(parent))
  stopifnot(is.character(vars))
  stopifnot(is.character(groups))

  structure(
    list(
      parent = parent,
      vars = vars,
      groups = groups,
      env = env,
      ...
    ),
    class = c(class, "dtplyr_step")
  )
}

#' @export
dim.dtplyr_step <- function(x) {
  c(NA, length(x$vars))
}

#' @export
print.dtplyr_step <- function(x, ...) {
  cat_line(crayon::bold("Source: "), "local data table ", dplyr::dim_desc(x))
  cat_line(crayon::bold("Call:   "), expr_text(dt_call(x)))
  cat_line()
  cat_line(crayon::silver(
    "# Use as.data.table()/as.data.frame()/as.tibble() to access results"
  ))

  invisible(x)
}

#' @importFrom dplyr show_query
#' @export
show_query.dtplyr_step <- function(x) {
  dt_call(x)
}

is_step <- function(x) inherits(x, "dtplyr_step")

dt_eval <- function(x) {
  dt <- dt_source(x)

  env <- env(x$env, `_DT` = dt)
  quo <- new_quosure(dt_call(x), env)

  eval_tidy(quo)
}

dt_needs_copy <- function(x) {
  UseMethod("dt_needs_copy")
}

dt_source <- function(x) {
  while (!is.data.table(x)) {
    x <- x$parent
  }
  x
}

dt_call <- function(x, needs_copy = dt_needs_copy(x)) {
  UseMethod("dt_call")
}

capture_dots <- function(..., vars, .named = TRUE) {
  enexprs(...)
}

