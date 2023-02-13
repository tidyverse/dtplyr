compute_by <- function(by,
                       data,
                       ...,
                       by_arg = "by",
                       data_arg = "data",
                       error_call = caller_env()) {
  check_dots_empty0(...)

  by <- enquo(by)
  check_by(by, data, by_arg = by_arg, data_arg = data_arg, error_call = error_call)

  names <- eval_select_by(by, data, error_call = error_call)

  if (length(names) == 0) {
    uses_by <- FALSE
  } else {
    uses_by <- TRUE
  }

  new_by(uses_by = uses_by, names = names)
}

is_grouped_dt <- function(data) {
  !is_empty(group_vars(data))
}

check_by <- function(by,
                     data,
                     ...,
                     by_arg = "by",
                     data_arg = "data",
                     error_call = caller_env()) {
  check_dots_empty0(...)

  if (quo_is_null(by)) {
    return(invisible(NULL))
  }

  if (is_grouped_dt(data)) {
    message <- paste0(
      "Can't supply {.arg {by_arg}} when ",
      "{.arg {data_arg}} is a grouped data frame."
    )
    cli::cli_abort(message, call = error_call)
  }

  invisible(NULL)
}

eval_select_by <- function(by,
                           data,
                           error_call = caller_env()) {
  out <- tidyselect::eval_select(
    expr = by,
    data = data,
    allow_rename = FALSE,
    error_call = error_call
  )
  names(out)
}

new_by <- function(uses_by = FALSE, names = character()) {
  structure(list(uses_by = uses_by, names = names), class = "dtplyr_by")
}



