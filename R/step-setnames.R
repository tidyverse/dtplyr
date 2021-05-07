step_setnames <- function(x, ..., in_place, rename_groups = FALSE) {
  stopifnot(is_step(x))
  stopifnot(is_bool(in_place))
  stopifnot(is_bool(rename_groups))

  sim_data <- simulate_vars(x)
  locs <- tidyselect::eval_rename(expr(c(...)), sim_data)

  new_vars <- x$vars
  new_vars[locs] <- names(locs)

  x_to_rename <- x$vars[locs]
  if (any(duplicated(x_to_rename))) {
    vars <- set_names(locs, names(locs))
  } else {
    vars <- set_names(x_to_rename, names(locs))
    vars <- vars[vars != names(vars)]
  }

  if (length(vars) == 0) {
    return(x)
  }

  out <- step_call(x,
    "setnames",
    args = list(unname(vars), names(vars)),
    vars = new_vars,
    in_place = in_place
  )

  if (rename_groups) {
    groups <- rename_groups(x$groups, vars)
    out <- step_group(out, groups)
  }

  out
}
