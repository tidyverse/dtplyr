
dt_subset <- function(dt, i, j, env = parent.frame(), sd_cols = NULL) {
  env <- new.env(parent = env, size = 2L)
  env$`_dt` <- dt

  args <- list(
    i = if (missing(i)) missing_arg() else dt_replace(i),
    j = if (missing(j)) missing_arg() else dt_replace(j)
  )

  if (missing(j)) {
    # No need for `by` groups when subsetting rows
    call <- substitute(`_dt`[i], args)
  } else {
    env$`_vars` <- dplyr::group_vars(dt)
    call <- substitute(`_dt`[i, j, by = `_vars`], args)
    call$.SDcols = sd_cols
  }

  eval_bare(call, env)
}

dt_replace <- function(x) {
  if (is.atomic(x)) {
    x
  } else if (is.name(x)) {
    if (identical(x, quote(.))) {
      quote(.SD)
    } else {
      x
    }
  } else if (is.call(x)) {
    if (identical(x, quote(n()))) {
      quote(.N)
    } else {
      x[] <- lapply(x, dt_replace)
      x
    }
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x),
      call. = FALSE)
  }
}

commas <- function(...) paste0(..., collapse = ", ")

drop_last <- function(x) {
  if (length(x) <= 1L) {
    return(NULL)
  }
  x[-length(x)]
}

