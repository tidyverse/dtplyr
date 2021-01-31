step_subset <- function(parent,
                        vars = parent$vars,
                        groups = parent$groups,
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
    arrange = arrange,
    i = i,
    j = j,
    on = on,
    implicit_copy = !is.null(i) || !is.null(j),
    class = "dtplyr_step_subset"
  )
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

  # grouped filters have to be performed first
  if (length(x$groups) > 0 && !is.null(x$i)) {
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

  if (length(x$groups) == 0) {
    if (is.null(i) && is.null(x$j)) {
      out <- parent
    } else if (is.null(i) && !is.null(x$j)) {
      out <- call2("[", parent, , x$j)
    } else if (!is.null(i) && is.null(x$j)) {
      out <- call2("[", parent, i)
    } else {
      out <- call2("[", parent, i, x$j)
    }
  } else {
    if (is.null(i)) {
      out <- call2("[", parent, , x$j)
    } else {
      if (is.null(x$j)) {
        j <- call2("[", expr(.SD), i)
      } else {
        j <- call2("[", expr(.SD), i, x$j)
      }
      out <- call2("[", parent, , j)
    }

    out <- add_grouping_param(out, x)
  }
  if (length(x$on) > 0) {
    out$on <- call2(".", !!!syms(x$on))
    out$allow.cartesian <- TRUE
  }
  out
}

# dplyr methods -----------------------------------------------------------

#' Subset columns using their names
#'
#' This is a method for the dplyr [select()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::select
#' @importFrom dplyr select
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(data.frame(x1 = 1, x2 = 2, y1 = 3, y2 = 4))
#'
#' dt %>% select(starts_with("x"))
#' dt %>% select(ends_with("2"))
#' dt %>% select(z1 = x1, z2 = x2)
select.dtplyr_step <- function(.data, ...) {
  sim_data <- simulate_vars(.data)
  locs <- tidyselect::eval_select(expr(c(...)), sim_data)
  locs <- ensure_group_vars(locs, .data$vars, .data$groups)

  vars <- set_names(.data$vars[locs], names(locs))

  if (length(vars) == 0) {
    j <- 0L
    groups <- .data$groups
  } else {
    groups <- rename_groups(.data$groups, vars)
    vars <- simplify_names(vars)
    j <- call2(".", !!!syms(vars))
  }

  out <- step_subset_j(.data, vars = names(locs), groups = character(), j = j)
  step_group(out, groups)
}

simulate_vars <- function(x) {
  as_tibble(rep_named(x$vars, list(logical())))
}

ensure_group_vars <- function(loc, names, groups) {
  group_loc <- match(groups, names)
  missing <- setdiff(group_loc, loc)

  if (length(missing) > 0) {
    vars <- names[missing]
    inform(paste0(
      "Adding missing grouping variables: ",
      paste0("`", names[missing], "`", collapse = ", ")
    ))
    loc <- c(set_names(missing, vars), loc)
  }

  loc
}

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
    # Suppress data.table warning when filteirng with a logical variable
    i <- call2("(", dots[[1]])
  } else {
    i <- Reduce(function(x, y) call2("&", x, y), dots)
  }

  step_subset(.data, i = i)
}

#' Arrange rows by column values
#'
#' This is a method for dplyr generic [arrange()]. It is translated to
#' an [order()] call in the `i` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams dplyr::arrange
#' @importFrom dplyr arrange
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#' dt %>% arrange(vs, cyl)
#' dt %>% arrange(desc(vs), cyl)
#' dt %>% arrange(across(mpg:disp))
arrange.dtplyr_step <- function(.data, ..., .by_group = FALSE) {
  dots <- capture_dots(.data, ..., .j = FALSE)
  if (.by_group) {
    dots <- c(syms(.data$groups), dots)
  }

  if (length(dots) == 0) {
    return(.data)
  }

  # Order without grouping then restore
  step <- step_subset(.data, i = call2("order", !!!dots), groups = character())
  step_group(step, groups = .data$groups)
}


#' Subset rows using their positions
#'
#' This is a method for the dplyr [slice()] generic. It is translated to
#' the `i` argument of `[.data.table`.
#'
#' @importFrom dplyr slice
#' @param .data A [lazy_dt()].
#' @param ... Positive integers giving rows to select, or negative
#'   integers giving rows to drop.
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(mtcars)
#' dt %>% slice(1, 5, 10)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   slice(1)
#'
#' dt %>%
#'   group_by(cyl) %>%
#'   slice(-1)
slice.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(.data, ..., .j = FALSE)

  if (length(dots) == 0) {
    i <- NULL
  } else if (length(dots) == 1) {
    i <- dots[[1]]
  } else {
    i <- call2("c", !!!dots)
  }

  step_subset(.data, i = i)
}

#' @importFrom dplyr sample_n
#' @export
sample_n.dtplyr_step <- function(tbl,
                                 size,
                                 replace = FALSE,
                                 weight = NULL
                                 ) {
  weight <- enexpr(weight)
  step_subset(tbl, i = sample_call(size, replace, weight))
}

#' @importFrom dplyr sample_frac
#' @export
sample_frac.dtplyr_step <- function(tbl,
                                    size = 1,
                                    replace = FALSE,
                                    weight = NULL
                                    ) {
  weight <- enexpr(weight)
  step_subset(tbl, i = sample_call(expr(.N * !!size), replace, weight))
}

sample_call <- function(size, replace = FALSE, weight = NULL) {
  call <- expr(sample(.N, !!size))

  if (replace) {
    call$replace <- TRUE
  }
  call$prob <- weight
  call
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

rename_groups <- function(groups, vars) {
  old2new <- set_names(names(vars), vars)
  groups[groups %in% names(old2new)] <- old2new[groups]
  groups
}

simplify_names <- function(vars) {
  names(vars)[vars == names(vars)] <- ""
  vars
}

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
