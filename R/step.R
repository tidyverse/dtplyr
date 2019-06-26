# We use a hybrid approach where most of the computation is done on
# construction. This avoids the deeply recursive approach of dbplyr, which
# should improve performance because we're not repeatedly re-computing the
# same values.
#
# dt_call() is managed separately because it involves much more code (which
# which dilute the intent of the constructor), and should only be called
# relatively few times.

new_step <- function(parent,
                     vars = parent$vars,
                     groups = parent$groups,
                     implicit_copy = parent$implicit_copy,
                     needs_copy = parent$needs_copy,
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
      implicit_copy = implicit_copy,
      needs_copy = needs_copy,
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
tbl_vars.dtplyr_step <- function(x) {
  x$vars
}

#' @export
as.data.table.dtplyr_step <- function(x, keep.rownames = FALSE, key = NULL, ...) {
  dt_eval(x)
}

#' @export
as.data.frame.dtplyr_step <- function(x, ...) {
  as.data.frame(dt_eval(x))
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.dtplyr_step <- function(x, ...) {
  as_tibble(dt_eval(x))
}

#' @export
#' @importFrom dplyr collect
collect.dtplyr_step <- function(x, ...) {
  dt_eval(x)
}

#' @export
print.dtplyr_step <- function(x, ...) {
  cat_line(crayon::bold("Source: "), "local data table ", dplyr::dim_desc(x))
  cat_line(crayon::bold("Call:   "), expr_text(dt_call(x)))
  cat_line()
  cat_line(format(as_tibble(head(x)))[-1]) # Hack to remove "A tibble" line
  cat_line()
  cat_line(crayon::silver(
    "# Use as.data.table()/as.data.frame()/as_tibble() to access results"
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
  env <- as_environment(dt_sources(x), x$env)
  quo <- new_quosure(dt_call(x), env)

  eval_tidy(quo)
}

# Returns a named list of data.tables: most just dispatch to their
# parent. The only exceptions are dt_step_first() and the two-table verbs.
dt_sources <- function(x) {
  UseMethod("dt_sources")
}
dt_sources.dtplyr_step <- function(x) {
  dt_sources(x$parent)
}

dt_call <- function(x, needs_copy = x$needs_copy) {
  UseMethod("dt_call")
}

capture_dots <- function(..., vars, .named = TRUE) {
  dots <- enexprs(..., .named = .named)
}

