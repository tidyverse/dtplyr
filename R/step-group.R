step_group <- function(parent, groups = parent$groups) {
  new_step(
    parent,
    vars = parent$vars,
    groups = groups,
    class = "dtplyr_step_group"
  )
}

dt_call.dtplyr_step_group <- function(x, needs_copy = x$needs_copy) {
  dt_call(x$parent, needs_copy)
}

# dplyr methods -----------------------------------------------------------

#' @export
group_by.dtplyr_step <- function(.data, ..., add = FALSE) {
  prep <- dplyr::group_by_prepare(.data, ..., add = add)

  step_group(
    prep$data,
    groups = prep$group_names
  )
}

#' @export
ungroup.dtplyr_step <- function(.data, ...) {
  step_group(.data, groups = character())
}
