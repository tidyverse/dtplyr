step_group <- function(parent, groups = parent$groups, keyed = parent$keyed) {
  new_step(
    parent,
    vars = parent$vars,
    groups = groups,
    class = "dtplyr_step_group",
    keyed = if (!is.null(keyed)) keyed else TRUE
  )
}

#' Helper type to handle keyby/by
#'
#' @param x The previous step
#' @return A list containing either keyby or by
by_struct <- function(x) {
  if (length(x$groups) > 0) {
    group_call <- call2(".", !!!syms(x$groups))
  } else {
    group_call <- NULL
  }

  structure(
    list(
      call = group_call,
      with = if (identical(x$keyed, FALSE)) "by" else "keyby"
    ),
    class = "by_struct"
  )
}

as.list.by_struct <- function(x) {
  ret <- list()
  
  if (!is.null(x$call)) {
    ret[[x$with]] <- x$call
  }

  ret
}

link_by_struct <- function(step, struct) {
  stopifnot(inherits(struct, "by_struct"))

  if (!is.null(struct$call)) {
    step[[struct$with]] <- struct$call
  }

  step
}

# dplyr methods -----------------------------------------------------------

#' @importFrom dplyr group_by
#' @export
group_by.dtplyr_step <- function(.data, ..., add = FALSE, key = TRUE) {
  dots <- capture_dots(.data, ...)

  existing <- vapply(dots, is_symbol, logical(1))
  if (!all(existing)) {
    .data <- mutate(.data, !!!dots[!existing])
    dots[!existing] <- syms(names(dots[!existing]))
  }

  groups <- c(if (add) .data$groups, names(dots))
  keyed <- if (!is.null(.data$keyed)) .data$keyed && key else key

  step_group(.data, groups, keyed)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dtplyr_step <- function(.data, ...) {
  step_group(.data, groups = character())
}

