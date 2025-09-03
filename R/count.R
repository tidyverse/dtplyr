#' Count observations by group
#'
#' This is a method for the dplyr [dplyr::count()] generic. It is translated using
#' `.N` in the `j` argument, and supplying groups to `keyby` as appropriate.
#'
#' @param x A [lazy_dt()]
#' @inheritParams dplyr::count
#' @importFrom dplyr count
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(dplyr::starwars)
#' dt %>% count(species)
#' dt %>% count(species, sort = TRUE)
#' dt %>% count(species, wt = mass, sort = TRUE)
count.dtplyr_step <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE)
    .groups <- "drop"
  } else {
    out <- x
    .groups <- "keep"
  }

  out <- tally_count(out, {{ wt }}, sort, name, .groups)

  out
}

#' @importFrom dplyr add_count
#' @export
add_count.dtplyr_step <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE)
  } else {
    out <- x
  }
  out <- dplyr::add_tally(out, wt = !!enquo(wt), sort = sort, name = name)
  out <- group_by(out, !!!syms(group_vars(x)))
  out
}

#' @importFrom dplyr tally
#' @export
tally.dtplyr_step <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  tally_count(x, {{ wt }}, sort, name, "drop_last")
}

# Helpers -----------------------------------------------------------------

tally_count <- function(.data, wt = NULL, sort = FALSE, name = NULL, .groups = "drop_last") {
  wt <- enquo(wt)
  if (quo_is_null(wt)) {
    n <- expr(n())
  } else {
    n <- expr(sum(!!wt, na.rm = TRUE))
  }
  name <- check_name(name, .data$groups)

  out <- summarise(.data, !!name := !!n, .groups = .groups)

  if (sort) {
    out <- arrange(out, desc(!!sym(name)))
  }

  out
}

check_name <- function(name, vars) {
  if (is.null(name)) {
    name <- n_name(vars)

    if (name != "n") {
      inform(c(
        glue::glue("Storing counts in `{name}`, as `n` already present in input"),
        i = "Use `name = \"new_name\"` to pick a new name."
      ))
    }
  } else if (!is_string(name)) {
    abort("`name` must be a string")
  }

  name
}

n_name <- function(x) {
  name <- "n"
  while (name %in% x) {
    name <- paste0("n", name)
  }

  name
}
