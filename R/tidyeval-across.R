dt_squash_across <- function(call, env, vars, j = j) {
  call <- match.call(dplyr::across, call, expand.dots = FALSE, envir = env)

  tbl <- as_tibble(rep_named(vars, list(logical())))
  locs <- tidyselect::eval_select(call$.cols, tbl, allow_rename = FALSE)
  cols <- syms(vars)[locs]

  # special case everything() to lapply(.SD)?

  funs <- across_funs(call$.fns, env)

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
  names(out) <- across_names(vars[locs], names(funs), .names, env)
  out
}

across_funs <- function(funs, env = caller_env()) {
  if (is.null(funs)) {
    list(function(x, ...) x)
  } else if (is_symbol(funs)) {
    set_names(list(across_fun(funs, env)), as.character(funs))
  } else if (is.character(funs)) {
    names(funs)[names2(funs) == ""] <- funs
    lapply(funs, across_fun, env)
  } else if (is_call(funs, "~")) {
    set_names(list(across_fun(funs, env)), expr_name(f_rhs(funs)))
  } else if (is_call(funs, "list")) {
    args <- rlang::exprs_auto_name(funs[-1])
    lapply(args, across_fun, env)
  } else if (!is.null(env)) {
    # Try evaluating once, just in case
    funs <- eval(funs, env)
    across_funs(funs, NULL)
  } else {
    abort("`.fns` argument to dbplyr::across() must be a NULL, a function name, formula, or list")
  }
}

across_fun <- function(fun, env) {
  if (is_symbol(fun) || is_string(fun)) {
    function(x, ...) call2(fun, x, ...)
  } else if (is_call(fun, "~")) {
    fun <- across_formula_fn(f_rhs(fun))
    function(x, ...) expr_interp(fun, child_env(emptyenv(), .x = x))
  } else {
    abort(c(
      ".fns argument to dbplyr::across() contain a function name or a formula",
      x = paste0("Problem with ", expr_deparse(fun))
    ))
  }
}


across_formula_fn <- function(x) {
  if (is_symbol(x, ".") || is_symbol(x, ".x")) {
    quote(!!.x)
  } else if (is_call(x)) {
    x[-1] <- lapply(x[-1], across_formula_fn)
    x
  } else {
    x
  }
}

across_names <- function(cols, funs, names = NULL, env = parent.frame()) {
  if (is.null(funs)) {
    return(NULL)
  }

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

find_fun <- function(fun) {
  if (is_lambda(fun)) {
    body <- body(fun)
    if (!is_call(body)) {
      return(NULL)
    }

    fun_name <- body[[1]]
    if (!is_symbol(fun_name)) {
      return(NULL)
    }

    as.character(fun_name)
  } else if (is.function(fun)) {
    fun_name(fun)
  }
}

fun_name <- function(fun) {
  pkg_env <- env_parent(global_env())
  known <- c(ls(base_agg), ls(base_scalar))

  for (x in known) {
    if (!env_has(pkg_env, x, inherit = TRUE))
      next

    fun_x <- env_get(pkg_env, x, inherit = TRUE)
    if (identical(fun, fun_x))
      return(x)
  }

  NULL
}
