step_group <- function(parent, groups = parent$groups, arrange = parent$arrange) {
  if (can_step_group_return_early(parent, groups, arrange)) {
    return(parent)
  }

  new_step(
    parent,
    vars = parent$vars,
    groups = groups,
    class = "dtplyr_step_group",
    arrange = arrange,
    name = parent$name
  )
}

#' @export
dt_has_computation.dtplyr_step_group <- function(x) {
  dt_has_computation(x$parent)
}


add_grouping_param <- function(call, step, arrange = step$arrange) {
  if (length(step$groups) == 0) {
    return(call)
  }

  arrange <- arrange %||% TRUE
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
  dots <- dots[!vapply(dots, is.null, logical(1))]

  if (lifecycle::is_present(add)) {
    lifecycle::deprecate_warn(
      "1.0.0",
      "dplyr::group_by(add = )",
      "group_by(.add = )"
    )
    .add <- add
  }

  existing <- vapply(
    seq_along(dots),
    function(i) {
      x <- dots[[i]]
      name <- names(dots)[[i]]
      is_symbol(x) && (as_name(x) == name)
    },
    logical(1)
  )

  if (!all(existing)) {
    .data <- mutate(.data, !!!dots[!existing])
    dots[!existing] <- syms(names(dots[!existing]))
  }

  groups <- c(if (.add) .data$groups, names(dots)) %||% character()
  arranged <- if (!is.null(.data$arrange)) .data$arrange && arrange else arrange

  step_group(.data, groups, arranged)
}

can_step_group_return_early <- function(parent, groups, arrange) {
  if (is_empty(groups)) {
    return(is_empty(parent$groups))
  }

  same_arrange <- (is_false(arrange) || identical(arrange, parent$arrange))
  same_groups <- identical(groups, parent$groups)
  same_arrange && same_groups
}

#' @export
group_by.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  group_by(.data, ...)
}

#' @importFrom dplyr ungroup
#' @export
#' @rdname group_by.dtplyr_step
ungroup.dtplyr_step <- function(.data, ...) {
  if (missing(...)) {
    step_group(.data, groups = character())
  } else {
    old_groups <- group_vars(.data)
    to_remove <- tidyselect::vars_select(.data$vars, ...)
    new_groups <- setdiff(old_groups, to_remove)
    step_group(.data, groups = new_groups)
  }
}

#' @export
ungroup.data.table <- function(.data, ...) {
  abort("Can't ungroup a data.table")
}

