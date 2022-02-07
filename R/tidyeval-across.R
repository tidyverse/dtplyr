capture_across <- function(data, x, j = TRUE) {
  x <- enquo(x)
  dt_squash_across(get_expr(x), get_env(x), data, j)
}

dt_squash_across <- function(call, env, data, j = j) {
  call <- match.call(dplyr::across, call, expand.dots = FALSE, envir = env)
  cols <- across_prepare_cols(data, call, env = env, allow_rename = TRUE)

  across_prepare_funs(data, call, cols, env, j)
}

capture_if_all <- function(data, x, j = TRUE) {
  x <- enquo(x)
  dt_squash_if(get_expr(x), get_env(x), data, j)
}

dt_squash_if <- function(call, env, data, j = j, reduce = "&") {
  call <- match.call(dplyr::if_any, call, expand.dots = FALSE, envir = env)
  cols <- across_prepare_cols(data, call, env = env, allow_rename = FALSE)

  if (is.null(call$.fns)) {
    return(Reduce(function(x, y) call2(reduce, x, y), cols))
  }

  out <- across_prepare_funs(data, call, cols, env, j)
  Reduce(function(x, y) call2(reduce, x, y), out)
}

across_prepare_cols <- function(data, call, env, allow_rename) {
  tbl <- simulate_vars(data, drop_groups = TRUE)
  .cols <- call$.cols %||% expr(everything())
  locs <- tidyselect::eval_select(.cols, tbl, env = env, allow_rename = allow_rename)

  cols <- syms(names(tbl))[locs]
  set_names(cols, names(locs))
}

across_prepare_funs <- function(data, call, cols, env, j) {
  dots <- call$...
  dots <- lapply(dots, dt_squash, env = env, data = data, j = j)

  funs <- across_funs(call$.fns, env, data, j = j, dots)

  # Generate grid of expressions
  out <- vector("list", length(cols) * length(funs))
  k <- 1
  for (i in seq_along(cols)) {
    for (j in seq_along(funs)) {
      out[[k]] <- exec(funs[[j]], cols[[i]])
      k <- k + 1
    }
  }

  .names <- eval(call$.names, env)
  names(out) <- across_names(names(cols), names(funs), .names, env)
  out
}


across_funs <- function(funs, env, data, j, dots) {
  if (is.null(funs)) {
    list(function(x, ...) x)
  } else if (is_symbol(funs) || is_function(funs)) {
    set_names(list(across_fun(funs, env, data, j = j, dots = dots)), as_label(funs))
  } else if (is.character(funs)) {
    names(funs)[names2(funs) == ""] <- funs
    lapply(funs, across_fun, env, data, j = j, dots = dots)
  } else if (is_call(funs, "~")) {
    set_names(list(across_fun(funs, env, data, j = j, dots = dots)), expr_name(f_rhs(funs)))
  } else if (is_call(funs, "list")) {
    args <- rlang::exprs_auto_name(funs[-1])
    lapply(args, across_fun, env, data, j = j, dots = dots)
  } else if (!is.null(env)) {
    # Try evaluating once, just in case
    funs <- eval(funs, env)
    across_funs(funs, NULL, j = j, dots = dots)
  } else {
    abort("`.fns` argument to dtplyr::across() must be a NULL, a function, formula, or list")
  }
}

across_fun <- function(fun, env, data, j, dots) {
  if (is_symbol(fun) || is_string(fun) ||
    is_call(fun, "function") || is_function(fun)) {
    function(x) call2(fun, x, !!!dots)
  } else if (is_call(fun, "~")) {
    has_dot_y <- call_has_dot_y(f_rhs(fun))
    if (has_dot_y && is_empty(dots)) {
      abort("object '.y' not found")
    }
    call <- dt_squash_formula(fun, env, data, j = j, replace_x = quote(!!.x),
                              replace_y = dots[[1]])
    function(x) expr_interp(call, child_env(emptyenv(), .x = x))
  } else {
    abort(c(
      ".fns argument to dtplyr::across() must contain a function or a formula",
      x = paste0("Problem with ", expr_deparse(fun))
    ))
  }
}

dt_squash_formula <- function(x, env, data, j = TRUE,
                              replace_x = quote(!!.x), replace_y = quote(!!.y)) {
  call <- f_rhs(x)
  call <- replace_dot(call, replace_x, replace_y)
  if (is_call(call)) {
    call <- dt_squash_call(call, env, data, j = j)
  }
  call
}

across_names <- function(cols, funs, names = NULL, env = parent.frame()) {
  n_reps <- if (is_empty(funs)) 1 else length(funs)
  if (n_reps == 1) {
    names <- names %||% "{.col}"
  } else {
    names <- names %||% "{.col}_{.fn}"
  }

  glue_env <- child_env(env,
    .col = rep(cols, each = n_reps),
    .fn = rep(funs %||% seq_len(n_reps), length(cols))
  )
  glue::glue(names, .envir = glue_env)
}
