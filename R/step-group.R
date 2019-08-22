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
  dots <- capture_dots(.data, ...)

  existing <- vapply(dots, is_symbol, logical(1))
  if (!all(existing)) {
    .data <- mutate(.data, !!!dots[!existing])
    dots[!existing] <- syms(names(dots[!existing]))
  }

  groups <- c(if (add) .data$groups, names(dots))
  step_group(.data, groups)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dtplyr_step <- function(.data, ...) {
  step_group(.data, groups = character())
}
