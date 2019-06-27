step_group <- function(parent, groups = parent$groups) {
  new_step(
    parent,
    vars = parent$vars,
    groups = groups,
    class = "dtplyr_step_group"
  )
}

# dplyr methods -----------------------------------------------------------

#' @importFrom dplyr group_by
#' @export
group_by.dtplyr_step <- function(.data, ..., add = FALSE) {
  dots <- capture_dots(...)
  # TODO: handle mutate semantics
  # prep <- dplyr::group_by_prepare(.data, !!!dots, add = add)

  step_group(.data, groups = names(dots))
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dtplyr_step <- function(.data, ...) {
  step_group(.data, groups = character())
}
