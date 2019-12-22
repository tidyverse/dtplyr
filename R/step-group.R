step_group <- function(parent, groups = parent$groups, keyby = parent$keyby) {
  new_step(
    parent,
    vars = parent$vars,
    groups = groups,
    class = "dtplyr_step_group",
    keyby = keyby
  )
}

by_struct <- function(x) {
  if (length(x$groups) > 0) {
    group_call <- call2(".", !!!syms(x$groups))
  } else {
    group_call <- NULL
  }

  structure(
    list(
      call  = group_call,
      using = if (identical(x$keyed, FALSE)) "by" else "keyby"
    ),
    class = "by_struct"
  )
}

as.list.by_struct <- function(x) {
  ret <- list()
  
  if (!is.null(x$call)) {
    ret[[x$using]] <- x$call
  }

  ret
}

link_by_struct <- function(call, struct) {
  stopifnot(inherits(struct, "by_struct"))

  if (!is.null(struct$call)) {
    call[[struct$using]] <- struct$call
  }

  call
}

add_grouping_parameter <- function(call, groups, keyby) {
  if (length(groups) == 0) {
    return(call)
  }

  using <- if (isTRUE(keyby) || is.null(keyby)) "keyby" else "by"

  call[[using]] <- call2(".", !!!syms(groups))

  call
}

# dplyr methods -----------------------------------------------------------

#' @importFrom dplyr group_by
#' @export
group_by.dtplyr_step <- function(.data, ..., add = FALSE, arrange = TRUE) {
  dots <- capture_dots(.data, ...)

  existing <- vapply(dots, is_symbol, logical(1))
  if (!all(existing)) {
    .data <- mutate(.data, !!!dots[!existing])
    dots[!existing] <- syms(names(dots[!existing]))
  }

  groups <- c(if (add) .data$groups, names(dots))
  arranged <- if (!is.null(.data$keyby)) .data$keyby && arrange else arrange

  step_group(.data, groups, arranged)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.dtplyr_step <- function(.data, ...) {
  step_group(.data, groups = character())
}

