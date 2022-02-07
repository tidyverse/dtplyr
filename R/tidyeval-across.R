capture_across <- function(data, x, j = TRUE) {
  x <- enquo(x)
  dt_squash_across(get_expr(x), get_env(x), data, j)
}

dt_squash_across <- function(call, env, data, j = j) {
  call <- match.call(dplyr::across, call, expand.dots = FALSE, envir = env)
  across_setup(data, call, env, allow_rename = TRUE, j = j)
}

capture_if_all <- function(data, x, j = TRUE) {
  x <- enquo(x)
  dt_squash_if(get_expr(x), get_env(x), data, j)
}

dt_squash_if <- function(call, env, data, j = j, reduce = "&") {
  call <- match.call(dplyr::if_any, call, expand.dots = FALSE, envir = env)
  out <- across_setup(data, call, env, allow_rename = FALSE, j = j)
  Reduce(function(x, y) call2(reduce, x, y), out)
}

across_funs <- function(funs, env, data, j, dots, names_spec = NULL) {
  if (is.null(funs)) {
    fns <- list(`1` = function(x, ...) x)
    names_spec <- names_spec %||% "{.col}"
    return(list(fns = fns, names = names_spec))
  } else if (is_symbol(funs) || is_function(funs)) {
    fns <- list(`1` = across_fun(funs, env, data, j = j, dots = dots))
    names_spec <- names_spec %||% "{.col}"
  } else if (is_call(funs, "~")) {
    fns <- list(`1` = across_fun(funs, env, data, j = j, dots = dots))
    names_spec <- names_spec %||% "{.col}"
  } else if (is_call(funs, "list")) {
    args <- rlang::call_args(funs)
    fns <- lapply(args, across_fun, env, data, j = j, dots = dots)
    names_spec <- names_spec %||% "{.col}_{.fn}"
  } else if (!is.null(env)) {
    # Try evaluating once, just in case
    funs <- eval(funs, env)
    return(across_funs(funs, NULL, j = j, dots = dots))
  } else {
    abort("`.fns` argument to dtplyr::across() must be a NULL, a function, formula, or list")
  }

  list(fns = fns, names = names_spec)
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

across_setup <- function(data,
                         call,
                         env,
                         allow_rename,
                         j) {
  tbl <- simulate_vars(data, drop_groups = TRUE)
  .cols <- call$.cols %||% expr(everything())
  locs <- tidyselect::eval_select(.cols, tbl, env = env, allow_rename = allow_rename)

  vars <- syms(names(tbl))[locs]
  if (allow_rename) {
    names_vars <- names(locs)
  } else {
    names_vars <- names(tbl)[locs]
  }

  dots <- lapply(call$..., dt_squash, env = env, data = data, j = j)
  names_spec <- eval(call$.names, env)
  funs_across_data <- across_funs(call$.fns, env, data, j = j, dots, names_spec)
  fns_is_null <- funs_across_data$fns_is_null
  fns <- funs_across_data$fns
  names_spec <- funs_across_data$names

  # make sure fns has names, use number to replace unnamed
  names_fns <- names2(fns)
  empties <- which(names_fns == "")
  if (length(empties)) {
    names_fns[empties] <- empties
  }

  glue_mask <- across_glue_mask(env,
    .col = rep(names_vars, each = length(fns)),
    .fn  = rep(names_fns , length(vars))
  )
  names_out <- vctrs::vec_as_names(glue(names_spec, .envir = glue_mask), repair = "check_unique")

  across_apply_fns(vars, fns, names_out)
}

across_apply_fns <- function(vars, fns, names) {
  out <- vector("list", length(vars) * length(fns))
  out <- set_names(out, names)
  k <- 1
  for (i in seq_along(vars)) {
    for (j in seq_along(fns)) {
      out[[k]] <- exec(fns[[j]], vars[[i]])
      k <- k + 1
    }
  }
  out
}

across_glue_mask <- function(.col, .fn, .caller_env) {
  glue_mask <- env(.caller_env, .col = .col, .fn = .fn)
  # TODO: we can make these bindings louder later
  env_bind_active(
    glue_mask, col = function() glue_mask$.col, fn = function() glue_mask$.fn
  )
  glue_mask
}
