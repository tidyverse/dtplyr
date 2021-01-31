capture_across <- function(data, x, j = TRUE) {
  x <- enquo(x)
  dt_squash_across(get_expr(x), get_env(x), data, j)
}

dt_squash_across <- function(call, env, data, j = j) {
  call <- match.call(dplyr::across, call, expand.dots = FALSE, envir = env)

  tbl <- simulate_vars(data, drop_groups = TRUE)
  locs <- tidyselect::eval_select(call$.cols, tbl, allow_rename = FALSE)
  cols <- syms(names(tbl))[locs]

  funs <- across_funs(call$.fns, env, data, j = j)

  # Generate grid of expressions
  out <- vector("list", length(cols) * length(funs))
  k <- 1
  for (i in seq_along(cols)) {
    for (j in seq_along(funs)) {
      out[[k]] <- exec(funs[[j]], cols[[i]], !!!call$...)
      k <- k + 1
    }
  }

  .names <- eval(call$.names, env)
  if (!is.null(call$.fns)) {
    names(out) <- across_names(names(tbl)[locs], names(funs), .names, env)
  }
  out
}

across_funs <- function(funs, env, data, j = TRUE) {
  if (is.null(funs)) {
    list(function(x, ...) x)
  } else if (is_symbol(funs)) {
    set_names(list(across_fun(funs, env, data, j = j)), as.character(funs))
  } else if (is.character(funs)) {
    names(funs)[names2(funs) == ""] <- funs
    lapply(funs, across_fun, env, data, j = j)
  } else if (is_call(funs, "~")) {
    set_names(list(across_fun(funs, env, data, j = j)), expr_name(f_rhs(funs)))
  } else if (is_call(funs, "list")) {
    args <- rlang::exprs_auto_name(funs[-1])
    lapply(args, across_fun, env, data, j = j)
  } else if (!is.null(env)) {
    # Try evaluating once, just in case
    funs <- eval(funs, env)
    across_funs(funs, NULL)
  } else {
    abort("`.fns` argument to dtplyr::across() must be a NULL, a function name, formula, or list")
  }
}

across_fun <- function(fun, env, data, j = TRUE) {
  if (is_symbol(fun) || is_string(fun)) {
    function(x, ...) call2(fun, x, ...)
  } else if (is_call(fun, "~")) {
    call <- f_rhs(fun)
    call <- replace_dot(call, quote(!!.x))
    call <- dt_squash_call(call, env, data, j = TRUE)

    function(x, ...) expr_interp(call, child_env(emptyenv(), .x = x))
  } else {
    abort(c(
      ".fns argument to dtplyr::across() contain a function name or a formula",
      x = paste0("Problem with ", expr_deparse(fun))
    ))
  }
}

across_names <- function(cols, funs, names = NULL, env = parent.frame()) {
  if (length(funs) == 1) {
    names <- names %||% "{.col}"
  } else {
    names <- names %||% "{.col}_{.fn}"
  }

  glue_env <- child_env(env,
    .col = rep(cols, each = length(funs)),
    .fn = rep(funs %||% seq_along(funs), length(cols))
  )
  glue::glue(names, .envir = glue_env)
}
