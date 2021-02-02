

#' Subset rows using their positions
#'
#' This is a method for the dplyr [slice()] generic. It is translated to
#' the `i` argument of `[.data.table`.
#'
#' @importFrom dplyr slice
#' @param .data A [lazy_dt()].
#' @param ... Positive integers giving rows to select, or negative
#'   integers giving rows to drop.
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#' dt %>% slice(1, 5, 10)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   slice(1)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   slice(-1)
slice.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(.data, ..., .j = FALSE)

  if (length(dots) == 0) {
    i <- NULL
  } else if (length(dots) == 1) {
    i <- dots[[1]]
  } else {
    i <- call2("c", !!!dots)
  }

  step_subset_i(.data, i)
}

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

sample_call <- function(size, replace = FALSE, weight = NULL) {
  call <- expr(sample(.N, !!size))

  if (replace) {
    call$replace <- TRUE
  }
  call$prob <- weight
  call
}

