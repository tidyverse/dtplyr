

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
#' @inheritParams dplyr::slice
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
slice.dtplyr_step <- function(.data, ..., .by = NULL) {
  dots <- capture_dots(.data, ..., .j = FALSE)
  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")

  if (length(dots) == 0) {
    i <- NULL
  } else {
    if (length(dots) == 1) {
      .rows <- dots[[1]]
    } else {
      .rows <- call2("c", !!!dots)
    }
    # Update logic once data.table #4353 is merged
    # https://github.com/Rdatatable/data.table/pull/4353
    assign_rows_var <- expr(.rows <- !!.rows)
    subset_valid_rows <- expr(.rows[between(.rows, -.N, .N)])
    i <- call2("{", assign_rows_var, subset_valid_rows)
  }

  step_subset_i(.data, i, by)
}

#' @rdname slice.dtplyr_step
#' @importFrom dplyr slice_head
#' @inheritParams dplyr::slice
#' @export
slice_head.dtplyr_step <- function(.data, ..., n, prop, by = NULL) {
  check_dots_empty()
  by <- compute_by({{ by }}, .data, by_arg = "by", data_arg = ".data")
  size <- get_slice_size(n, prop, "slice_head")
  i <- expr(rlang::seq2(1L, !!size))
  step_subset_i(.data, i = i, by)
}

#' @rdname slice.dtplyr_step
#' @importFrom dplyr slice_tail
#' @export
slice_tail.dtplyr_step <- function(.data, ..., n, prop, by = NULL) {
  check_dots_empty()
  by <- compute_by({{ by }}, .data, by_arg = "by", data_arg = ".data")
  size <- get_slice_size(n, prop, "slice_tail")
  i <- expr(rlang::seq2(.N - !!size + 1L, .N))
  step_subset_i(.data, i = i, by)
}

#' @rdname slice.dtplyr_step
#' @importFrom dplyr slice_min
#' @inheritParams dplyr::slice
#' @export
slice_min.dtplyr_step <- function(.data,
                                  order_by,
                                  ...,
                                  n,
                                  prop,
                                  by = NULL,
                                  with_ties = TRUE) {
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
    by = {{ by }},
    with_ties = with_ties,
    .slice_fn = "slice_min"
  )
}

#' @rdname slice.dtplyr_step
#' @importFrom dplyr slice_max
#' @export
slice_max.dtplyr_step <- function(.data,
                                  order_by,
                                  ...,
                                  n,
                                  prop,
                                  by = NULL,
                                  with_ties = TRUE) {
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
    by = {{ by }},
    with_ties = with_ties,
    .slice_fn = "slice_max"
  )
}

slice_min_max <- function(.data,
                          order_by,
                          decreasing,
                          ...,
                          n,
                          prop,
                          by = NULL,
                          with_ties = TRUE,
                          .slice_fn = "slice_min_max") {
  check_dots_empty()
  size <- get_slice_size(n, prop, .slice_fn)

  by <- compute_by({{ by }}, .data, by_arg = "by", data_arg = ".data")

  order_by <- capture_dot(.data, {{ order_by }}, j = FALSE)

  if (decreasing) {
    order_by <- expr(desc(!!order_by))
  }

  if (with_ties) {
    ties.method <- "min"
  } else {
    ties.method <- "first"
  }

  i <- expr(!!smaller_ranks(!!order_by, !!size, ties.method = ties.method))

  out <- step_subset_i(.data, i, by)
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
  check_dots_empty()
  size <- get_slice_size(n, prop, "slice_sample")

  wt <- enexpr(weight_by)

  i <- sample_int(.N, !!size, replace = replace, wt = wt)

  step_subset_i(.data, i)
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
                                 weight = NULL,
                                 .env = NULL,
                                 ...
                                 ) {
  weight <- enexpr(weight)
  step_subset_i(tbl, i = sample_call(size, replace, weight))
}

#' @importFrom dplyr sample_frac
#' @export
sample_frac.dtplyr_step <- function(tbl,
                                    size = 1,
                                    replace = FALSE,
                                    weight = NULL,
                                    .env = NULL,
                                    ...
                                    ) {
  weight <- enexpr(weight)
  step_subset_i(tbl, i = sample_call(expr(.N * !!size), replace, weight))
}


# helpers -----------------------------------------------------------------

check_constant <- function(x, name, fn) {
  withCallingHandlers(force(x), error = function(e) {
    abort(c(
      glue("`{name}` must be a constant in `{fn}()`."),
      x = conditionMessage(e)
    ), parent = e)
  })
}

check_slice_size <- function(n, prop, .slice_fn = "check_slice_size", call = caller_env()) {
  if (missing(n) && missing(prop)) {
    list(type = "n", n = 1L)
  } else if (!missing(n) && missing(prop)) {
    n <- check_constant(n, "n", .slice_fn)
    if (!is.numeric(n) || length(n) != 1 || is.na(n)) {
      abort("`n` must be a single number.", call = call)
    }
    list(type = "n", n = as.integer(n))
  } else if (!missing(prop) && missing(n)) {
    prop <- check_constant(prop, "prop", .slice_fn)
    if (!is.numeric(prop) || length(prop) != 1 || is.na(prop)) {
      abort("`prop` must be a single number.", call = call)
    }
    list(type = "prop", prop = prop)
  } else {
    abort("Must supply exactly one of `n` and `prop` arguments.", call = call)
  }
}

get_slice_size <- function(n, prop, .slice_fn = "get_slice_size") {
  slice_input <- check_slice_size(n, prop, .slice_fn, call = caller_env())

  if (slice_input$type == "n") {
    if (slice_input$n < 0) {
      expr(max(.N + !!slice_input$n, 0L))
    } else {
      expr(min(!!slice_input$n, .N))
    }
  } else if (slice_input$type == "prop") {
    if (slice_input$prop < 0) {
      expr(max(.N + as.integer(!!slice_input$prop * .N), 0L))
    } else {
      expr(min(as.integer(!!slice_input$prop * .N), .N))
    }
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

