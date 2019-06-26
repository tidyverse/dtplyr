new_step_mutate <- function(parent,
                            new_vars = list(),
                            groups = parent$groups) {

  vars <- union(parent$vars, names(new_vars))

  new_step(
    parent,
    vars = vars,
    groups = groups,
    needs_copy = !parent$implicit_copy,
    new_vars = new_vars,
    class = "dtplyr_step_mutate"
  )
}

dt_call.dtplyr_step_mutate <- function(x, needs_copy = x$needs_copy) {
  # i is always empty because we never mutate a subset
  j <- call2(":=", !!!x$new_vars)
  out <- call2("[", dt_call(x$parent, needs_copy), , j)

  if (length(x$groups) > 0) {
    out$by <- call2(".", !!!syms(x$groups))
  }
  out
}

# dplyr methods -----------------------------------------------------------

#' @export
mutate.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(...)

  nest_vars(.data, dots, .data$vars, transmute = FALSE)
}

#' @importFrom dplyr transmute
#' @export
transmute.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(...)

  nest_vars(.data, dots, .data$vars, transmute = TRUE)
}


# For each expression, check if it uses any newly created variables.
# If so, nest the mutate()
nest_vars <- function(.data, dots, all_vars, transmute = FALSE) {
  new_vars <- character()
  all_new_vars <- unique(names(dots))

  init <- 0L
  for (i in seq_along(dots)) {
    cur_var <- names(dots)[[i]]
    used_vars <- all_names(get_expr(dots[[i]]))

    if (any(used_vars %in% new_vars)) {
      .data <- new_step_mutate(.data, dots[new_vars])
      all_vars <- c(all_vars, setdiff(new_vars, all_vars))
      new_vars <- cur_var
      init <- i
    } else {
      new_vars <- c(new_vars, cur_var)
    }
  }

  if (init != 0L) {
    dots <- dots[-seq2(1L, init - 1)]
  }

  if (transmute) {
    # Final step needs to include all variable names
    vars <- syms(set_names(all_new_vars))
    vars[names(dots)] <- dots
    names(vars)[!names(vars) %in% names(dots)] <- ""

    new_step_subset(.data, j = call2(".", !!!vars))
  } else {
    new_step_mutate(.data, dots)
  }
}

# Helpers -----------------------------------------------------------------

all_names <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (is_quosure(x)) return(all_names(quo_get_expr(x)))
  if (!is.call(x)) return(NULL)

  unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
}
