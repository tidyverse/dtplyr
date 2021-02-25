#' Replace NAs with specified values
#'
#' @description
#' This is a method for the tidyr `replace_na()` generic. It is translated to
#' [data.table::fcoalesce()].
#'
#' Note that unlike `tidyr::replace_na()`, `data.table::fcoalesce()` cannot
#' replace `NULL` values in lists.
#'
#' @inheritParams tidyr::replace_na
#' @param data A [lazy_dt()].
#' @examples
#' library(tidyr)
#'
#' # Replace NAs in a data frame
#' dt <- lazy_dt(tibble(x = c(1, 2, NA), y = c("a", NA, "b")))
#' dt %>% replace_na(list(x = 0, y = "unknown"))
#'
#' # Replace NAs using `dplyr::mutate()`
#' dt %>% dplyr::mutate(x = replace_na(x, 0))
# exported onLoad
replace_na.dtplyr_step <- function(data, replace = list()) {

  stopifnot(is.list(replace))
  if (length(replace) == 0) {
    return(data)
  }

  sim_data <- simulate_vars(data)
  replace_vars <- intersect(names(replace), names(sim_data))

  replace_calls <- vector("list", length(replace_vars))
  names(replace_calls) <- replace_vars

  for (i in seq_along(replace_vars)) {
    var <- replace_vars[[i]]
    check_replacement(replace[[i]], var)
    replace_calls[[i]] <- call2("fcoalesce", sym(var), replace[[i]])
  }

  mutate(data, !!!replace_calls)
}

# exported onLoad
replace_na.data.table <- function(data, replace = list()) {
  data <- lazy_dt(data)
  tidyr::replace_na(data, replace = replace)
}

check_replacement <- function(x, var) {
  n <- length(x)
  if (n == 1) {
    return()
  }

  abort(glue::glue("Replacement for `{var}` is length {n}, not length 1"))
}
