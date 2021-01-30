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

#' Group and ungroup
#'
#' These are methods for dplyr's [group_by()] and [ungroup()] generics.
#' Grouping is translated to the either `keyby` and `by` argument of
#' `[.data.table` depending on the value of the `arrange` argument.
#'
#' @inheritParams dplyr::group_by
#' @param .data A [lazy_dt()]
#' @param arrange If `TRUE`, will automatically arrange the output of
#'   subsequent grouped operations by group. If `FALSE`, output order will be
#'   left unchanged. In the generated data.table code this switches between
#'   using the `keyby` (`TRUE`) and `by` (`FALSE`) arguments.
#' @param .add,add When `FALSE`, the default, `group_by()` will
#'   override existing groups. To add to the existing groups, use
#'   `.add = TRUE`.
#'
#'   This argument was previously called `add`, but that prevented
#'   creating a new grouping variable called `add`, and conflicts with
#'   our naming conventions.
#' @importFrom dplyr group_by
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' dt <- lazy_dt(mtcars)
#'
#' # group_by() is usually translated to `keyby` so that the groups
#' # are ordered in the output
#' dt %>%
#'  group_by(cyl) %>%
#'  summarise(mpg = mean(mpg))
#'
#' # use `arrange = FALSE` to instead use `by` so the original order
#' # or groups is preserved
#' dt %>%
#'  group_by(cyl, arrange = FALSE) %>%
#'  summarise(mpg = mean(mpg))
group_by.dtplyr_step <- function(.data, ..., .add = FALSE, add = deprecated(), arrange = TRUE) {
  dots <- capture_dots(.data, ...)
  dots <- exprs_auto_name(dots)

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
#' @rdname group_by.dtplyr_step
ungroup.dtplyr_step <- function(.data, ...) {
  step_group(.data, groups = character())
}

