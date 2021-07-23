

#' Subset rows using their positions
#'
#' @description
#' These are methods for the dplyr [slice()], `slice_head()`, `slice_tail()`,
#' `slice_min()`, `slice_max()` and `slice_sample()` generics. They are
#' translated to the `i` argument of `[.data.table`.
#'
#' Unlike dplyr, `slice()` (and `slice()` alone) returns the same number of
#' rows per group, regardless of whether or not the indices appear in each
#' group.
#'
#' @importFrom dplyr slice
#' @param .data A [lazy_dt()].
#' @param n,prop Provide either `n`, the number of rows, or `prop`, the
#'   proportion of rows to select. If neither are supplied, `n = 1` will be
#'   used.
#'
#'   If `n` is greater than the number of rows in the group (or `prop > 1`),
#'   the result will be silently truncated to the group size. If the
#'   `prop`ortion of a group size is not an integer, it is rounded down.
#' @param ... Positive integers giving rows to select, or negative
#'   integers giving rows to drop.
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#' dt %>% slice(1, 5, 10)
#' dt %>% slice(-(1:4))
#'
#' # First and last rows based on existing order
#' dt %>% slice_head(n = 5)
#' dt %>% slice_tail(n = 5)
#'
#' # Rows with minimum and maximum values of a variable
#' dt %>% slice_min(mpg, n = 5)
#' dt %>% slice_max(mpg, n = 5)
#'
#' # slice_min() and slice_max() may return more rows than requested
#' # in the presence of ties. Use with_ties = FALSE to suppress
#' dt %>% slice_min(cyl, n = 1)
#' dt %>% slice_min(cyl, n = 1, with_ties = FALSE)
#'
#' # slice_sample() allows you to random select with or without replacement
#' dt %>% slice_sample(n = 5)
#' dt %>% slice_sample(n = 5, replace = TRUE)
#'
#' # you can optionally weight by a variable - this code weights by the
#' # physical weight of the cars, so heavy cars are more likely to get
#' # selected
#' dt %>% slice_sample(weight_by = wt, n = 5)
slice.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(.data, ..., .j = FALSE)

  if (length(dots) == 0) {
    i <- NULL
  } else {
    if (length(dots) == 1) {
      .rows <- dots[[1]]
    } else {
      .rows <- call2("c", !!!dots)
    }
    between <- call2("between", .rows, quote(-.N), quote(.N))
    i <- call2("[", .rows, between)
  }

  step_subset_i(.data, i)
}

#' @export
slice.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  slice(.data, ...)
}

#' @rdname slice.dtplyr_step
#' @importFrom dplyr slice_head
#' @inheritParams dplyr::slice
#' @export
slice_head.dtplyr_step <- function(.data, ..., n, prop) {
  ellipsis::check_dots_empty()
  size <- check_slice_size(n, prop)
  i <- switch(size$type,
    n = expr(seq.int(min(!!size$n, .N))),
    prop = expr(seq.int(!!size$prop * .N)),
  )
  step_subset_i(.data, i = i)
}

#' @export
slice_head.data.table <- function(.data, ..., n, prop) {
  .data <- lazy_dt(.data)
  slice_head(.data, ..., n = n, prop = prop)
}

#' @rdname slice.dtplyr_step
#' @importFrom dplyr slice_tail
#' @export
slice_tail.dtplyr_step <- function(.data, ..., n, prop) {
  ellipsis::check_dots_empty()
  size <- check_slice_size(n, prop)
  n_sequence <- switch(size$type,
    n = expr(min(!!size$n, .N)),
    prop = expr(!!size$prop * .N),
  )
  step_subset_i(.data, i = expr(seq.int(.N - !!n_sequence + 1, .N)))
}

#' @export
slice_tail.data.table <- function(.data, ..., n, prop) {
  .data <- lazy_dt(.data)
  slice_tail(.data, ..., n = n, prop = prop)
}

#' @rdname slice.dtplyr_step
#' @importFrom dplyr slice_min
#' @inheritParams dplyr::slice
#' @export
slice_min.dtplyr_step <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) {
    abort("argument `order_by` is missing, with no default.")
  }

  slice_min_max(
    .data,
    order_by = {{ order_by }},
    decreasing = FALSE,
    ...,
    n =  n,
    prop = prop,
    with_ties = with_ties
  )
}

#' @export
slice_min.data.table <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  .data <- lazy_dt(.data)
  slice_min(.data, {{ order_by }}, ..., n = n, prop = prop, with_ties = with_ties)
}

#' @rdname slice.dtplyr_step
#' @importFrom dplyr slice_max
#' @export
slice_max.dtplyr_step <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  if (missing(order_by)) {
    abort("argument `order_by` is missing, with no default.")
  }

  slice_min_max(
    .data,
    order_by = {{ order_by }},
    decreasing = TRUE,
    ...,
    n =  n,
    prop = prop,
    with_ties = with_ties
  )
}

#' @export
slice_max.data.table <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  .data <- lazy_dt(.data)
  slice_max(.data, {{ order_by }}, ..., n = n, prop = prop, with_ties = with_ties)
}

slice_min_max <- function(.data, order_by, decreasing, ..., n, prop, with_ties = TRUE) {
  ellipsis::check_dots_empty()
  size <- check_slice_size(n, prop)

  order_by <- capture_dot(.data, {{ order_by }}, j = FALSE)

  if (decreasing) {
    order_by <- expr(desc(!!order_by))
  }

  if (with_ties) {
    ties.method <- "min"
  } else {
    ties.method <- "first"
  }

  i <- switch(size$type,
    n = expr(!!smaller_ranks(!!order_by, !!size$n, ties.method = ties.method)),
    prop = expr(!!smaller_ranks(!!order_by, !!size$prop * .N, ties.method = ties.method))
  )

  out <- step_subset_i(.data, i)
  arrange(out, !!order_by, .by_group = TRUE)
}

smaller_ranks <- function(x, y, ties.method = "min") {
  x <- enexpr(x)
  y <- enexpr(y)

  # `frank()` by group is much slower than rank
  # https://github.com/Rdatatable/data.table/issues/3988
  # also https://github.com/Rdatatable/data.table/issues/4284
  expr(rank(!!x, ties.method = !!ties.method, na.last = "keep") <= !!y)
}

#' @importFrom dplyr slice_sample
#' @inheritParams dplyr::slice
#' @export
slice_sample.dtplyr_step <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  size <- check_slice_size(n, prop)
  ellipsis::check_dots_empty()

  wt <- enexpr(weight_by)

  i <- switch(size$type,
    n =    sample_int(.N, !!size$n, replace = replace, wt = wt),
    prop = sample_int(.N, !!size$prop * .N, replace = replace, wt = wt),
  )

  step_subset_i(.data, i)
}

#' @export
slice_sample.data.table <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  .data <- lazy_dt(.data)
  slice_sample(.data, ..., n = n, prop = prop, weight_by = !!enexpr(weight_by), replace = replace)
}

sample_int <- function(n, size, replace = FALSE, wt = NULL) {
  n <- enexpr(n)
  size <- enexpr(size)

  if (replace) {
    out <- expr(sample.int(!!n, !!size, replace = TRUE))
  } else {
    out <- expr(sample.int(!!n, min(!!size, !!n)))
  }

  if (!is.null(wt)) {
    out$prob <- wt
  }

  out
}


# sample_ -----------------------------------------------------------------

#' @importFrom dplyr sample_n
#' @export
sample_n.dtplyr_step <- function(tbl,
                                 size,
                                 replace = FALSE,
                                 weight = NULL
                                 ) {
  weight <- enexpr(weight)
  step_subset_i(tbl, i = sample_call(size, replace, weight))
}

#' @export
sample_n.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  sample_n(.data, ...)
}

#' @importFrom dplyr sample_frac
#' @export
sample_frac.dtplyr_step <- function(tbl,
                                    size = 1,
                                    replace = FALSE,
                                    weight = NULL
                                    ) {
  weight <- enexpr(weight)
  step_subset_i(tbl, i = sample_call(expr(.N * !!size), replace, weight))
}

#' @export
sample_frac.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  sample_frac(.data, ...)
}


# helpers -----------------------------------------------------------------

check_slice_size <- function(n, prop) {
  if (missing(n) && missing(prop)) {
    list(type = "n", n = 1L)
  } else if (!missing(n) && missing(prop)) {
    if (!is.numeric(n) || length(n) != 1) {
      abort("`n` must be a single number.")
    }
    if (is.na(n) || n < 0) {
      abort("`n` must be a non-missing positive number.")
    }

    list(type = "n", n = as.integer(n))
  } else if (!missing(prop) && missing(n)) {
    if (!is.numeric(prop) || length(prop) != 1) {
      abort("`prop` must be a single number")
    }
    if (is.na(prop) || prop < 0) {
      abort("`prop` must be a non-missing positive number.")
    }
    list(type = "prop", prop = prop)
  } else {
    abort("Must supply exactly one of `n` and `prop` arguments.")
  }
}

sample_call <- function(size, replace = FALSE, weight = NULL) {
  call <- expr(sample(.N, !!size))

  if (replace) {
    call$replace <- TRUE
  }
  call$prob <- weight
  call
}

