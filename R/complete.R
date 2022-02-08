#' Complete a data frame with missing combinations of data
#'
#' @description
#' This is a method for the tidyr `complete()` generic. This is a wrapper
#' around `dtplyr` translations for `expand()`, `full_join()`, and `replace_na()`
#' that's useful for completing missing combinations of data.
#'
#' @param data A [lazy_dt()].
#' @inheritParams tidyr::complete
#' @examples
#' library(tidyr)
#' tbl <- tibble(x = 1:2, y = 1:2, z = 3:4)
#' dt <- lazy_dt(tbl)
#'
#' dt %>%
#'   complete(x, y)
#'
#' dt %>%
#'   complete(x, y, fill = list(z = 10L))
# exported onLoad
complete.dtplyr_step <- function(data, ..., fill = list()) {
  dots <- enquos(...)
  dots <- dots[!map_lgl(dots, quo_is_null)]
  if (length(dots) == 0) {
    return(data)
  }

  full <- tidyr::expand(data, !!!dots)
  full <- dplyr::full_join(full, data, by = full$vars)
  full <- tidyr::replace_na(full, replace = fill)
  full
}

# exported onLoad
complete.data.table <- function(data, ..., fill = list()) {
  data <- lazy_dt(data)
  tidyr::complete(data, ..., fill = fill)
}
