
#' Subset rows using column values
#'
#' This is a method for the dplyr [arrange()] generic. It is translated to
#' the `i` argument of `[.data.table`
#'
#' @param .data A [lazy_dt()].
#' @param .preserve Ignored
#' @inheritParams dplyr::filter
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#' dt %>% filter(cyl == 4)
#' dt %>% filter(vs, am)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   filter(mpg > mean(mpg))
#' @importFrom dplyr filter
# exported onLoad
filter.dtplyr_step <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_filter(...)
  dots <- capture_dots(.data, ..., .j = FALSE)

  by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")

  if (filter_by_lgl_col(dots)) {
    # Suppress data.table warning when filtering with a logical variable
    i <- call2("(", dots[[1]])
  } else {
    i <- Reduce(function(x, y) call2("&", x, y), dots)
  }

  step_subset_i(.data, i, by)
}

filter_by_lgl_col <- function(dots) {
  if (length(dots) > 1) {
    return(FALSE)
  }

  dot <- dots[[1]]
  if (is_symbol(dot)) {
    return(TRUE)
  }

  # catch expressions of form `!x`
  is_call(dot, name = "!", n = 1) && is_symbol(dot[[2]])
}

check_filter <- function(...) {
  dots <- enquos(...)
  named <- have_name(dots)

  for (i in which(named)) {
    quo <- dots[[i]]

    # only allow named logical vectors, anything else
    # is suspicious
    expr <- quo_get_expr(quo)
    if (!is.logical(expr)) {
      abort(c(
        glue::glue("Problem with `filter()` input `..{i}`."),
        x = glue::glue("Input `..{i}` is named."),
        i = glue::glue("This usually means that you've used `=` instead of `==`."),
        i = glue::glue("Did you mean `{name} == {as_label(expr)}`?", name = names(dots)[i])
      ), call = caller_env())
    }

  }
}
