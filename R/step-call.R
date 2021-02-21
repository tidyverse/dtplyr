step_call <- function(parent, fun, args = list(), vars = parent$vars, in_place = FALSE) {

  stopifnot(is_step(parent))
  stopifnot(is.character(fun))
  stopifnot(is.list(args))

  new_step(
    parent = parent,
    vars = vars,
    groups = parent$groups,
    implicit_copy = !in_place,
    needs_copy = in_place || parent$needs_copy,
    fun = fun,
    args = args,
    class = "dtplyr_step_call"
  )
}

dt_call.dtplyr_step_call <- function(x, needs_copy = x$needs_copy) {
  call2(x$fun, dt_call(x$parent, needs_copy), !!!x$args)
}

# dplyr verbs -------------------------------------------------------------

#' Subset first or last rows
#'
#' These are methods for the base generics [head()] and [tail()]. They
#' are not translated.
#'
#' @param x A [lazy_dt()]
#' @param n Number of rows to select. Can use a negative number to instead
#'   drop rows from the other end.
#' @param ... Passed on to [head()]/[tail()].
#' @importFrom utils head
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' dt <- lazy_dt(data.frame(x = 1:10))
#'
#' # first three rows
#' head(dt, 3)
#' # last three rows
#' tail(dt, 3)
#'
#' # drop first three rows
#' tail(dt, -3)
head.dtplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "head", args = list(n = n))
}

#' @importFrom utils tail
#' @export
#' @rdname head.dtplyr_step
tail.dtplyr_step <- function(x, n = 6L, ...) {
  step_call(x, "tail", args = list(n = n))
}


#' Rename columns using their names
#'
#' These are methods for the dplyr generics [rename()] and [rename_with()].
#' They are both translated to [data.table::setnames()].
#'
#' @param .data A [lazy_dt()]
#' @inheritParams dplyr::rename
#' @importFrom dplyr rename
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' dt <- lazy_dt(data.frame(x = 1, y = 2, z = 3))
#' dt %>% rename(new_x = x, new_y = y)
#' dt %>% rename_with(toupper)
rename.dtplyr_step <- function(.data, ...) {
  sim_data <- simulate_vars(.data)
  locs <- tidyselect::eval_rename(expr(c(...)), sim_data)

  new_vars <- .data$vars
  new_vars[locs] <- names(locs)

  vars <- set_names(.data$vars[locs], names(locs))
  vars <- vars[vars != names(vars)]

  if (length(vars) == 0) {
    return(.data)
  }

  out <- step_call(.data,
    "setnames",
    args = list(unname(vars), names(vars)),
    vars = new_vars,
    in_place = TRUE
  )

  groups <- rename_groups(.data$groups, vars)
  step_group(out, groups)
}


#' @export
rename.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  rename(.data, ...)
}

#' @importFrom dplyr rename_with
#' @importFrom tidyselect everything
#' @rdname rename.dtplyr_step
#' @export
rename_with.dtplyr_step <- function(.data, .fn, .cols = everything(), ...) {
  if (!missing(...)) {
    abort("`dtplyr::rename_with() doesn't support ...")
  }

  fn_expr <- enexpr(.fn)

  if (is_symbol(fn_expr)) {
    fn <- fn_expr
  } else if (is_string(fn_expr)) {
    fn <- sym(fn_expr)
  } else if (is_call(fn_expr, "~")) {
    env <- caller_env()
    call <- dt_squash_formula(
      fn_expr,
      env = env,
      data = .data,
      j = FALSE,
      replace = quote(x)
    )
    fn <- new_function(exprs(x =), call, env)
  } else {
    abort("`.fn` must be a function name or formula")
  }
  # Still have to compute the new variable names for the table metadata
  # But this should be fast, so doing it twice shouldn't matter
  .fn <- as_function(.fn)

  sim_data <- simulate_vars(.data)
  locs <- unname(tidyselect::eval_select(enquo(.cols), sim_data))
  old_vars <- .data$vars[locs]
  new_vars <- .fn(old_vars)

  vars <- .data$vars
  vars[locs] <- new_vars

  if (identical(locs, seq_along(sim_data))) {
    out <- step_call(.data,
      "setnames",
      args = list(fn),
      vars = vars,
      in_place = TRUE
    )
  } else {
    out <- step_call(.data,
      "setnames",
      args = list(old_vars, fn),
      vars = vars,
      in_place = TRUE
    )
  }

  groups <- rename_groups(.data$groups, set_names(new_vars, old_vars))
  step_group(out, groups)
}

#' @export
rename_with.data.table <- function(.data, .fn, .cols = everything(), ...) {
  .data <- lazy_dt(.data)
  rename_with(.data, .fn = .fn, .cols = {{.cols}}, ...)
}

#' Subset distinct/unique rows
#'
#' This is a method for the dplyr [distinct()] generic. It is translated to
#' [data.table::unique.data.table()].
#'
#' @importFrom dplyr distinct
#' @param .data A [lazy_dt()]
#' @inheritParams dplyr::distinct
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' df <- lazy_dt(data.frame(
#'   x = sample(10, 100, replace = TRUE),
#'   y = sample(10, 100, replace = TRUE)
#' ))
#'
#' df %>% distinct(x)
#' df %>% distinct(x, y)
#' df %>% distinct(x, .keep_all = TRUE)
distinct.dtplyr_step <- function(.data, ..., .keep_all = FALSE) {
  dots <- capture_dots(.data, ...)

  if (length(dots) > 0) {
    only_syms <- all(vapply(dots, is_symbol, logical(1)))

    if (.keep_all) {
      if (only_syms) {
        by <- union(.data$groups, names(dots))
      } else {
        .data <- mutate(.data, !!!dots)
        by <- names(.data$new_vars)
      }
    } else {
      if (only_syms) {
        .data <- select(.data, !!!dots)
      } else {
        .data <- transmute(.data, !!!dots)
      }
      by <- NULL
    }
  } else {
    by <- NULL
  }

  args <- list()
  args$by <- by

  step_call(.data, "unique", args = args)
}

#' @export
distinct.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  distinct(.data, ...)
}

#' @export
unique.dtplyr_step <- function(x, incomparables = FALSE, ...) {
  if (!missing(incomparables)) {
    abort("`incomparables` not supported by `unique.dtplyr_step()`")
  }
  distinct(x)
}

# tidyr verbs -------------------------------------------------------------
#' Drop rows containing missing values
#'
#' @description
#' This is a method for the tidyr `drop_na()` generic. It is translated to
#' `data.table::na.omit()`
#'
#' @param data A [lazy_dt()].
#' @inheritParams tidyr::drop_na
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' dt <- lazy_dt(tibble(x = c(1, 2, NA), y = c("a", NA, "b")))
#' dt %>% drop_na()
#' dt %>% drop_na(x)
#'
#' vars <- "y"
#' dt %>% drop_na(x, any_of(vars))
# exported onLoad
drop_na.dtplyr_step <- function(data, ...) {
  sim_data <- simulate_vars(data)
  locs <- names(tidyselect::eval_select(expr(c(...)), sim_data))

  args <- list()
  if (length(locs) > 0) {
    args$cols <- locs
  }

  step_call(data, "na.omit", args = args)
}

# exported onLoad
drop_na.data.table <- function(data, ...) {
  data <- lazy_dt(data)
  tidyr::drop_na(data, ...)
}
