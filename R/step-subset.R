step_subset <- function(parent,
                        vars = parent$vars,
                        groups = parent$groups,
                        locals = parent$locals,
                        arrange = parent$arrange,
                        i = NULL,
                        j = NULL,
                        on = character()
                        ) {

  stopifnot(is_step(parent))
  stopifnot(is.null(i) || is_expression(i) || is_step(i))
  stopifnot(is.null(j) || is_expression(j))
  stopifnot(is.character(on))

  new_step(
    parent = parent,
    vars = vars,
    groups = groups,
    locals = locals,
    arrange = arrange,
    i = i,
    j = j,
    on = on,
    implicit_copy = !is.null(i) || !is.null(j),
    class = "dtplyr_step_subset"
  )
}

# Grouped i needs an intermediate assignment for maximum efficiency
step_subset_i <- function(parent, i) {
  if (length(parent$groups) > 0) {
    parent <- compute(parent)

    nm <- sym(parent$name)
    i <- expr((!!nm)[, .I[!!i]])              # dt[, .I[]]
    i <- add_grouping_param(i, parent, FALSE) # dt[, .I[], by = ()]
    i <- call("$", i, quote(V1))              # dt[, .I[], by = ()]$V1
  }

  step_subset(parent, i = i)
}

# When adding a subset that contains only j, it may be possible to merge
# the previous step.
step_subset_j <- function(parent,
                          vars = parent$vars,
                          groups = parent$groups,
                          arrange = parent$arrange,
                          j = NULL) {
  if (can_merge_subset(parent)) {
    i <- parent$i
    on <- parent$on
    parent <- parent$parent
  } else {
    i <- NULL
    on <- character()
  }

  step_subset(
    parent,
    vars = vars,
    groups = groups,
    arrange = arrange,
    i = i,
    j = j,
    on = on
  )
}

can_merge_subset <- function(x) {
  # Can only merge subsets
  if (!inherits(x, "dtplyr_step_subset")) {
    return(FALSE)
  }

  # Don't need to check that groups are identical because the only
  # dplyr functions that generate expression in i are
  # filter/slice/sample/arrange/join and don't affect groups

  is.null(x$j)
}

#' @export
dt_sources.dtplyr_step_subset <- function(x) {
  # TODO: need to throw error if same name refers to different tables.
  if (is_step(x$i)) {
    utils::modifyList(dt_sources(x$parent), dt_sources(x$i))
  } else {
    dt_sources(x$parent)
  }
}

#' @export
dt_call.dtplyr_step_subset <- function(x, needs_copy = x$needs_copy) {
  if (is.null(x$i) && is.null(x$j)) {
    return(dt_call(x$parent))
  }

  i <- if (is_step(x$i)) dt_call(x$i) else x$i

  parent <- dt_call(x$parent, needs_copy)

  if (is.null(i) && is.null(x$j)) {
    out <- parent
  } else if (is.null(i) && !is.null(x$j)) {
    out <- call2("[", parent, , x$j)
  } else if (!is.null(i) && is.null(x$j)) {
    out <- call2("[", parent, i)
  } else {
    out <- call2("[", parent, i, x$j)
  }

  if (!is.null(x$j)) {
    out <- add_grouping_param(out, x)
  }

  if (length(x$on) > 0) {
    out$on <- call2(".", !!!syms(x$on))
    out$allow.cartesian <- TRUE
  }
  out
}

# dplyr methods -----------------------------------------------------------

#' Summarise each group to one row
#'
#' This is a method for the dplyr [summarise()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::summarise
#' @importFrom dplyr summarise
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   summarise(vs = mean(vs))
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   summarise(across(disp:wt, mean))
summarise.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(.data, ...)
  check_summarise_vars(dots)

  if (length(dots) == 0) {
    if (length(.data$groups) == 0) {
      out <- step_subset_j(.data, vars = character(), j = 0L)
    } else {
      # Acts like distinct on grouping vars
      out <- distinct(.data, !!!syms(.data$groups))
    }
  } else {
    out <- step_subset_j(
      .data,
      vars = union(.data$groups, names(dots)),
      j = call2(".", !!!dots)
    )
  }

  step_group(out, groups = head(.data$groups, -1))
}

#' Create new columns, dropping old
#'
#' This is a method for the dplyr [transmute()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams mutate.dtplyr_step
#' @importFrom dplyr transmute
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(dplyr::starwars)
#' dt %>% transmute(name, sh = paste0(species, "/", homeworld))
transmute.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(.data, ...)
  nested <- nested_vars(.data, dots, .data$vars)

  if (!nested) {
    j <- call2(".", !!!dots)
  } else {
    assign <- Map(function(x, y) call2("<-", x, y), syms(names(dots)), dots)
    output <- call2(".", !!!syms(set_names(names(dots))))
    j <- call2("{", !!!assign, output)
  }
  step_subset_j(.data, vars = names(dots), j = j)
}

#' Count observations by group
#'
#' This is a method for the dplyr [count()] generic. It is translated using
#' `.N` in the `j` argument, and supplying groups to `keyby` as appropriate.
#'
#' @param .data A [lazy_dt()]
#' @inheritParams dplyr::count
#' @importFrom dplyr count
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#'
#' dt <- lazy_dt(dplyr::starwars)
#' dt %>% count(species)
#' dt %>% count(species, sort = TRUE)
#' dt %>% count(species, wt = mass, sort = TRUE)
count.dtplyr_step <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL) {
  if (!missing(...)) {
    out <- group_by(.data, ..., .add = TRUE)
  } else {
    out <- .data
  }

  wt <- enexpr(wt)
  if (is.null(wt)) {
    n <- expr(n())
  } else {
    n <- expr(sum(!!wt, na.rm = TRUE))
  }

  if (is.null(name)) {
    name <- "n"
  } else if (!is_string(name)) {
    abort("`name` must be a string")
  }

  out <- summarise(out, !!name := !!n)

  if (sort) {
    out <- arrange(out, desc(!!sym(name)))
  }

  out
}

#' Subset rows using column values
#'
#' This is a method for the dplyr [arrange()] generic. It is translated to
#' the `i` argument of `[.data.table`
#'
#' @param .data A [lazy_dt()].
#' @param .preserve Ignored
#' @inheritParams dplyr::filter
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#' dt %>% filter(cyl == 4)
#' dt %>% filter(vs, am)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   filter(mpg > mean(mpg))
# exported onLoad
filter.dtplyr_step <- function(.data, ..., .preserve = FALSE) {
  dots <- capture_dots(.data, ..., .j = FALSE)

  if (length(dots) == 1 && is_symbol(dots[[1]])) {
    # Suppress data.table warning when filtering with a logical variable
    i <- call2("(", dots[[1]])
  } else {
    i <- Reduce(function(x, y) call2("&", x, y), dots)
  }

  step_subset_i(.data, i)
}

#' @importFrom dplyr do
#' @export
do.dtplyr_step <- function(.data, ...) {
  # This is a partial implementation, because I don't think that many
  # people are likely to use it, given that do() is marked as questioning
  # Problems:
  # * doesn't handle unnamed case
  # * doesn't set .SDcols so `.SD` will only refer to non-groups
  # * can duplicating group vars (#5)

  dots <- capture_dots(.data, ...)

  if (any(names2(dots) == "")) {
    # I can't see any way to figure out what the variables are
    abort("Unnamed do() not supported by dtplyr")
  }

  new_vars <- lapply(dots, function(x) call2(".", x))
  j <- call2(".", !!!new_vars)

  vars <- union(.data$vars, names(dots))

  step_subset_j(.data, vars = vars, j = j)
}

# helpers ------------------------------------------------------------------

# For each expression, check if it uses any newly created variables
check_summarise_vars <- function(dots) {
  for (i in seq_along(dots)) {
    used_vars <- all_names(get_expr(dots[[i]]))
    cur_vars <- names(dots)[seq_len(i - 1)]

    if (any(used_vars %in% cur_vars)) {
      abort(paste0(
        "`", names(dots)[[i]], "` ",
        "refers to a variable created earlier in this summarise().\n",
        "Do you need an extra mutate() step?"
      ))
    }
  }
}
