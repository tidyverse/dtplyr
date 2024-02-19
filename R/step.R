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
                     locals = parent$locals,
                     implicit_copy = parent$implicit_copy,
                     needs_copy = parent$needs_copy,
                     env = parent$env,
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
      locals = locals,
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

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.dtplyr_step <- function(x) {
  x$vars
}

#' @importFrom dplyr groups
#' @export
groups.dtplyr_step <- function(x) {
  syms(x$groups)
}

#' @importFrom dplyr group_vars
#' @export
group_vars.dtplyr_step <- function(x) {
  x$groups
}

#' @importFrom dplyr group_size
#' @export
group_size.dtplyr_step <- function(x) {
  collect(summarise(x, n = .N))$n
}

#' @importFrom dplyr n_groups
#' @export
n_groups.dtplyr_step <- function(x) {
  length(group_size(x))
}

#' Force computation of a lazy data.table
#'
#' * `collect()` returns a tibble, grouped if needed.
#' * `compute()` generates an intermediate assignment in the translation.
#' * `as.data.table()` returns a data.table.
#' * `as.data.frame()` returns a data frame.
#' * `as_tibble()` returns a tibble.
#'
#' @export
#' @param x A [lazy_dt]
#' @param ... Arguments used by other methods.
#' @importFrom dplyr collect
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#'
#' # Generate translation
#' avg_mpg <- dt %>%
#'   filter(am == 1) %>%
#'   group_by(cyl) %>%
#'   summarise(mpg = mean(mpg))
#'
#' # Show translation and temporarily compute result
#' avg_mpg
#'
#' # compute and return tibble
#' avg_mpg_tb <- as_tibble(avg_mpg)
#' avg_mpg_tb
#'
#' # compute and return data.table
#' avg_mpg_dt <- data.table::as.data.table(avg_mpg)
#' avg_mpg_dt
#'
#' # modify translation to use intermediate assignment
#' compute(avg_mpg)
#'
collect.dtplyr_step <- function(x, ...) {
  # for consistency with dbplyr::collect()
  out <- as_tibble(x)

  if (length(x$groups) > 0) {
    out <- group_by(out, !!!syms(x$groups))
  }

  out
}

#' @rdname collect.dtplyr_step
#' @param name Name of intermediate data.table.
#' @export
#' @importFrom dplyr compute
compute.dtplyr_step <- function(x, name = unique_name(), ...) {
  if (!dt_has_computation(x)) {
    return(x)
  }

  step_locals(x, set_names(list(dt_call(x)), name), name)
}

#' @rdname collect.dtplyr_step
#' @export
#' @param keep.rownames Ignored as dplyr never preserves rownames.
as.data.table.dtplyr_step <- function(x, keep.rownames = FALSE, ...) {
  dt_eval(x)[]
}

#' @rdname collect.dtplyr_step
#' @export
as.data.frame.dtplyr_step <- function(x, ...) {
  as.data.frame(dt_eval(x))
}

#' @rdname collect.dtplyr_step
#' @export
#' @importFrom tibble as_tibble
#' @param .name_repair Treatment of problematic column names
as_tibble.dtplyr_step <- function(x, ..., .name_repair = "check_unique") {
  out <- as_tibble(dt_eval(x), .name_repair = .name_repair)
  attr(out, ".internal.selfref") <- NULL
  attr(out, "sorted") <- NULL
  out
}

#' @export
#' @importFrom dplyr pull
pull.dtplyr_step <- function(.data, var = -1, name = NULL, ...) {
  var <- sym(tidyselect::vars_pull(.data$vars, !!enquo(var)))

  .data <- ungroup(.data)

  name <- enquo(name)
  if (quo_is_null(name)) {
    .data <- select(.data, !! var)
    .data <- collect(.data)
    .data[[1]]
  } else {
    name <- sym(tidyselect::vars_pull(.data$vars, !!name))
    .data <- select(.data, !! var, !! name)
    .data <- collect(.data)
    set_names(.data[[1]], .data[[2]])
  }
}

#' @export
print.dtplyr_step <- function(x,
                              ...,
                              n = 6,
                              max_extra_cols = NULL,
                              max_footer_lines = NULL) {
  dt <- as.data.table(x)

  cat_line(cli::style_bold("Source: "), "local data table ", dplyr::dim_desc(dt))
  if (length(x$groups) > 0) {
    cat_line(cli::style_bold("Groups: "), paste(x$groups, collapse = ", "))
  }
  if (length(x$locals) > 0) {
    cat_line(cli::style_bold("Call:"))
    for (var in names(x$locals)) {
      cat_line("  ", var, " <- ", expr_deparse(x$locals[[var]]))
    }
    cat_line("  ", expr_text(dt_call(x)))
  } else {
    cat_line(cli::style_bold("Call:   "), expr_text(dt_call(x)))
  }
  cat_line()
  cat_line(
    format(
      as_tibble(dt, .name_repair = "minimal"),
      n = n,
      max_extra_cols = max_extra_cols,
      max_footer_lines = max_footer_lines
    )[-1]
  ) # Hack to remove "A tibble" line
  cat_line()
  cat_line(cli::col_silver(
    "# Use as.data.table()/as.data.frame()/as_tibble() to access results"
  ))

  invisible(x)
}

#' @importFrom dplyr glimpse
#' @export
glimpse.dtplyr_step <- function(x, width = NULL, ...) {
  glimpse(collect(x), width = width, ...)
}

#' @importFrom dplyr show_query
#' @export
show_query.dtplyr_step <- function(x, ...) {
  dt_call(x)
}

is_step <- function(x) inherits(x, "dtplyr_step")


# Returns a named list of data.tables: most just dispatch to their
# parent. The only exceptions are dt_step_first() and the two-table verbs.
dt_sources <- function(x) {
  UseMethod("dt_sources")
}
#' @export
dt_sources.dtplyr_step <- function(x) {
  dt_sources(x$parent)
}

dt_call <- function(x, needs_copy = x$needs_copy) {
  UseMethod("dt_call")
}
#' @export
dt_call.dtplyr_step <- function(x, needs_copy = x$needs_copy) {
  dt_call(x$parent, needs_copy)
}

dt_has_computation <- function(x) {
  UseMethod("dt_has_computation")
}
#' @export
dt_has_computation.dtplyr_step <- function(x) {
  TRUE
}
