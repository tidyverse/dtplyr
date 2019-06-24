new_step_group <- function(parent, vars = parent$vars, groups = parent$groups) {
  new_step(
    parent,
    vars = vars,
    groups = groups,
    class = "dtplyr_step_group"
  )
}

dt_call.dtplyr_step_group <- function(x) dt_call(x$parent)

# dplyr methods -----------------------------------------------------------

group_by.dtplyr_step <- function(.data, ..., add = FALSE) {
  prep <- dplyr::group_by_prepare(.data, ..., add = add)

  new_step_group(
    prep$data,
    groups = prep$group_names
  )
}

ungroup.dtplyr_step <- function(.data, ...) {
  new_step_group(.data, groups = character())
}
