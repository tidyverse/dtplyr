step_setnames <- function(x, old, new, in_place, rename_groups = FALSE) {
  stopifnot(is_step(x))
  stopifnot(is.character(old) || is.integer(old))
  stopifnot(is.character(new))
  stopifnot(length(old) == length(new))
  stopifnot(is_bool(in_place))
  stopifnot(is_bool(rename_groups))

  x_vars <- replace_setnames_na(x$vars)
  old <- replace_setnames_na(old)
  new <- replace_setnames_na(new)

  if (is.integer(old)) {
    locs <- old
  } else {
    locs <- vctrs::vec_match(old, x_vars)
  }

  name_changed <- x_vars[locs] != new
  old <- old[name_changed]
  new <- new[name_changed]
  locs <- locs[name_changed]

  if (length(old) == 0) {
    return(x)
  }

  new_vars <- x_vars
  new_vars[locs] <- new
  out <- step_call(x,
    "setnames",
    args = list(old, new),
    vars = new_vars,
    in_place = in_place
  )

  if (rename_groups) {
    groups <- rename_groups(x$groups, set_names(old, new))
    out <- step_group(out, groups)
  }

  out
}

# Replace character `NA` names with "NA"
replace_setnames_na <- function(x) {
  if (is.character(x)) {
    x <- vctrs::vec_assign(x, is.na(x), "NA")
  }
  x
}
