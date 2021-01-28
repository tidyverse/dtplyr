step_group <- function(parent, groups = parent$groups, arrange = parent$arrange) {
  new_step(
    parent,
    vars = parent$vars,
    groups = groups,
    class = "dtplyr_step_group",
    arrange = arrange
  )
}

add_grouping_param <- function(call, step) {
  if (length(step$groups) == 0) {
    return(call)
  }

  arrange <- if (!is.null(step$arrange)) step$arrange else TRUE
  using <- if (isTRUE(arrange)) "keyby" else "by"

  call[[using]] <- call2(".", !!!syms(step$groups))

  call
}

# dplyr methods -----------------------------------------------------------

#' @inheritParams dplyr::group_by
#' @param .data A data.table
#' @param arrange If `TRUE`, will automatically arrange the output of
#'   subsequent grouped operations by group. If `FALSE`, output order will be
#'   left unchanged. In the generated data.table code this switches between
#'   using the `keyby` (`TRUE`) and `by` (`FALSE`) arguments.
#' @importFrom dplyr group_by
#' @rdname single_table
#' @export
group_by.dtplyr_step <- function(.data, ..., .add = FALSE, add = deprecated(), arrange = TRUE) {
  dots <- capture_dots(.data, ...)

  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0",
      "dplyr::group_by(add = )",
      "group_by(.add = )"
    )
    .add <- add
  }

  existing <- vapply(dots, is_symbol, logical(1))
  if (!all(existing)) {
    .data <- mutate(.data, !!!dots[!existing])
    dots[!existing] <- syms(names(dots[!existing]))
  }

  groups <- c(if (.add) .data$groups, names(dots))
  arranged <- if (!is.null(.data$arrange)) .data$arrange && arrange else arrange

  step_group(.data, groups, arranged)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dtplyr_step <- function(.data, ...) {
  step_group(.data, groups = character())
}

