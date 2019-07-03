# nocov start

warn_data_frame_method <- function() {
  warn(strwrap(paste0(
    "You are using a dplyr method on a raw data.table, which will call the ",
    "data frame implementation, and is likely to be inefficient. \n\n",
    "To suppress this message, either generate a data.table translation ",
    "with `lazy_dt()` or convert to a data frame or tibble with ", "
    `as.data.frame()`/`as_tibble()`."
  )))
}

# Single table ------------------------------------------------------------

#' @export
distinct.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
do.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

# exported onLoad
filter.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
group_by.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
mutate.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
rename.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
sample_frac.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
sample_n.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
select.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
slice.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
summarise.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
transmute.data.table <- function(.data, ...) {
  warn_data_frame_method()
  NextMethod()
}


# Two-table ---------------------------------------------------------------

#' @export
left_join.data.table <- function(x, y, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
right_join.data.table <- function(x, y, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
inner_join.data.table <- function(x, y, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
full_join.data.table <- function(x, y, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
semi_join.data.table <- function(x, y, ...) {
  warn_data_frame_method()
  NextMethod()
}

#' @export
anti_join.data.table <- function(x, y, ...) {
  warn_data_frame_method()
  NextMethod()
}

# nocov end
