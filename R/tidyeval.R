dt_eval <- function(x) {
  env <- as_environment(dt_sources(x), x$env)
  add_dt_wrappers(env)

  for (var in names(x$locals)) {
    env[[var]] <- eval(x$locals[[var]], env)
  }

  quo <- new_quosure(dt_call(x), env)

  eval_tidy(quo)
}

# Make sure data.table functions are available so dtplyr still works
# even when data.table isn't attached
dt_funs <- c(
  "between", "CJ", "copy", "data.table", "dcast", "melt", "nafill",
  "fcase", "fcoalesce", "fifelse", "fintersect", "frank", "frankv", "fsetdiff", "funion",
  "rleid", "setcolorder", "setnames", "setorder", "shift", "tstrsplit", "uniqueN"
)
dt_symbols <- c(".SD", ".BY", ".N", ".I", ".GRP", ".NGRP")
add_dt_wrappers <- function(env) {
  env_bind(env, !!!env_get_list(ns_env("data.table"), dt_funs))
}
globalVariables(dt_funs)

# These functions attempt to simulate tidy eval as much as possible within
# data.table. The goal is to get the majority of real-world code to work,
# without aiming for 100% compliance.

capture_dots <- function(.data, ..., .j = TRUE, .by = new_by()) {
  if (.by$uses_by) {
    .data <- step_group(.data, .by$names)
  }

  dots <- enquos(..., .named = .j)
  dots <- map(dots, dt_squash, data = .data, j = .j, is_top = TRUE)

  # Remove names from any list elements
  is_list <- map_lgl(dots, is.list)
  names(dots)[is_list] <- ""

  # Auto-splice list results from dt_squash()
  dots[!is_list] <- lapply(dots[!is_list], list)
  unlist(dots, recursive = FALSE)
}

capture_new_vars <- function(.data, ..., .by = new_by()) {
  if (.by$uses_by) {
    .data <- step_group(.data, .by$names)
  }

  dots <- as.list(enquos(..., .named = TRUE))
  for (i in seq_along(dots)) {
    dot <- dots[[i]]
    dot <- dt_squash(dot, data = .data, is_top = TRUE)
    if (is.null(dot)) {
      dots[i] <- list(NULL)
    } else {
      dots[[i]] <- dot
    }
    .data$vars <- union(.data$vars, names(dot) %||% names(dots)[i])
  }

  # Remove names from any list elements
  is_list <- map_lgl(dots, is.list)
  names(dots)[is_list] <- ""

  # Auto-splice list results from dt_squash()
  dots[!is_list] <- lapply(dots[!is_list], list)
  unlist(dots, recursive = FALSE)
}

capture_dot <- function(.data, x, j = TRUE) {
  dt_squash(enquo(x), data = .data, j = j)
}

# squash quosures
dt_squash <- function(x, env, data, j = TRUE, is_top = FALSE) {
  if (is_atomic(x) || is_null(x)) {
    x
  } else if (is_symbol(x)) {
    if (identical(x, quote(.))) {
      quote(.SD)
    } else {
      var <- as.character(x)

      if (var %in% c("T", "F")) {
        as.logical(var)
      } else if (var %in% dt_symbols) {
        # data table pronouns are bound to NULL
        x
      } else if (!var %in% data$vars && env_has(env, var, inherit = TRUE)) {
        if (is_global(env)) {
          # Slightly dangerous because the variable might be modified
          # between creation and execution, but it seems like a reasonable
          # tradeoff in order to get a more natural translation.
          if (j) {
            # use .. to avoid data mask
            sym(paste0("..", var))
          } else {
            # i doesn't provide a data mask
            x
          }
        } else {
          eval(x, env)
        }
      } else {
        x
      }
    }
  } else if (is_quosure(x)) {
    dt_squash(get_expr(x), get_env(x), data, j = j, is_top)
  } else if (is_call(x, "if_any")) {
    dt_squash_if(x, env, data, j = j, reduce = "|")
  } else if (is_call(x, "if_all")) {
    dt_squash_if(x, env, data, j = j, reduce = "&")
  } else if (is_call(x, "across")) {
    dt_squash_across(x, env, data, j = j, is_top)
  } else if (is_call(x, "pick")) {
    x[[1]] <- sym("c")
    call <- call2("across", x)
    dt_squash_across(call, env, data, j, is_top)
  } else if (is_call(x)) {
    dt_squash_call(x, env, data, j = j)
  } else {
    abort("Invalid input")
  }
}

dt_squash_call <- function(x, env, data, j = TRUE) {
  if (is_mask_pronoun(x)) {
    var <- x[[3]]
    if (is_call(x, "[[")) {
      var <- sym(eval(var, env))
    }
    if (is_symbol(x[[2]], ".data")) {
      var
    } else if (is_symbol(x[[2]], ".env")) {
      sym(paste0("..", var))
    }
  } else if (is_call(x, c("coalesce", "replace_na"))) {
    args <- lapply(x[-1], dt_squash, env, data, j)
    call2("fcoalesce", !!!args)
  } else if (is_call(x, "case_when")) {
    # case_when(x ~ y) -> fcase(x, y)
    call <- call_match(x, dplyr::case_when)
    args <- call_args(call)
    if (!is.null(args$.ptype) || !is.null(args$.size)) {
      abort("`.ptype` and `.size` are not supported in dtplyr case_when")
    }
    if (!is.null(args$.default)) {
      args$.default <- call2("~", quote(rep(TRUE, .N)), args$.default)
      args <- unname(args)
    }
    args <- unlist(lapply(args, function(x) {
      list(
        # Get as "default" case as close as possible
        # https://github.com/Rdatatable/data.table/issues/4258
        if (isTRUE(x[[2]]) || is_symbol(x[[2]], "T")) quote(rep(TRUE, .N)) else x[[2]],
        x[[3]]
      )
    }))
    args <- lapply(args, dt_squash, env = env, data = data, j = j)
    call2("fcase", !!!args)
  } else if (is_call(x, "case_match")) {
    x <- call_match(x, dplyr::case_match, dots_expand = FALSE)
    args <- call_args(x)
    .x <- args$.x
    dots <- args$...
    dots <- map(dots, prep_case_match_dot, .x)
    call <- call2("case_when", !!!dots, .default = args$.default)
    dt_squash_call(call, env, data, j = j)
  } else if (is_call(x, "cur_data")) {
    quote(.SD)
  } else if (is_call(x, "cur_data_all")) {
    abort("`cur_data_all()` is not available in dtplyr")
  } else if (is_call(x, "cur_group")) {
    quote(.BY)
  } else if (is_call(x, "cur_group_id")) {
    quote(.GRP)
  } else if (is_call(x, "cur_group_rows")) {
    quote(.I)
  } else if (is_call(x, "desc")) {
    check_one_arg(x)
    x[[1]] <- sym("-")
    x[[2]] <- dt_squash(x[[2]], env, data, j)
    x
  } else if (is_call(x, c("if_else", "ifelse"))) {
    if (is_call(x, "if_else")) {
      x <- unname(call_match(x, dplyr::if_else))
    } else {
      x <- unname(call_match(x, ifelse))
    }

    x[[1]] <- quote(fifelse)
    x[-1] <- lapply(x[-1], dt_squash, env, data, j = j)
    x
  } else if (is_call(x, c("lag", "lead"))) {
    if (is_call(x, "lag")) {
      type <- "lag"
    } else {
      type <- "lead"
    }
    x <- call_match(x, dplyr::lag)
    x[-1] <- lapply(x[-1], dt_squash, env = env, data = data, j = j)

    args <- call_args(x)
    call <- call2("shift", args[[1]])
    if (!is_null(args$n)) {
      call$n <- args$n
    }
    if (!is_null(args$default)) {
      call$fill <- args$default
    }
    if (!is_null(args$order_by)) {
      abort(
        glue::glue("The `order_by` argument of `{type}()` is not supported by dtplyr")
      )
    }
    call$type <- type
    call
  } else if (is_call(x, "n", n = 0)) {
    quote(.N)
  } else if (is_call(x, "n_distinct")) {
    x <- call_match(x, dplyr::n_distinct, dots_expand = FALSE)
    dots <- x$...
    if (length(dots) == 1) {
      vec <- dots[[1]]
    } else {
      vec <- call2("data.table", !!!dots)
    }
    call <- call2("uniqueN", vec)
    if (!is_null(x$na.rm)) {
      call$na.rm <- x$na.rm
    }
    call
  } else if (is_call(x, "row_number", n = 0)) {
    quote(seq_len(.N))
  } else if (is_call(x, "row_number", n = 1)) {
    arg <- dt_squash(x[[2]], env, data, j = j)
    expr(frank(!!arg, ties.method = "first", na.last = "keep"))
  } else if (is_call(x, "min_rank")) {
    check_one_arg(x)
    arg <- dt_squash(x[[2]], env, data, j = j)
    expr(frank(!!arg, ties.method = "min", na.last = "keep"))
  } else if (is_call(x, "dense_rank")) {
    check_one_arg(x)
    arg <- dt_squash(x[[2]], env, data, j = j)
    expr(frank(!!arg, ties.method = "dense", na.last = "keep"))
  } else if (is_call(x, "percent_rank")) {
    check_one_arg(x)
    arg <- dt_squash(x[[2]], env, data, j = j)
    frank_expr <- expr((frank(!!arg, ties.method = "min", na.last = "keep") - 1))
    expr(!!frank_expr / (sum(!is.na(!!arg)) - 1))
  } else if (is_call(x, "cume_dist")) {
    check_one_arg(x)
    arg <- dt_squash(x[[2]], env, data, j = j)
    frank_expr <- expr(frank(!!arg, ties.method = "max", na.last = "keep"))
    expr(!!frank_expr / sum(!is.na(!!arg)))
  } else if (is.function(x[[1]]) || is_call(x, "function")) {
    simplify_function_call(x, env, data, j = j)
  } else if (is_call(x, c("glue", "str_glue")) && j) {
    call <- call_match(x, glue::glue)
    if (is.null(call$.envir)) {
      call$.envir <- quote(.SD)
    }
    call
  } else if (is_call(x, "consecutive_id")) {
    x[[1]] <- expr(rleid)
    x[-1] <- lapply(x[-1], dt_squash, env, data, j = j)
    x
  } else if (is_call(x, "$") && !is_mask_pronoun(x)) {
    x
  } else {
    x[-1] <- lapply(x[-1], dt_squash, env, data, j = j)
    x
  }
}

is_mask_pronoun <- function(x) {
  is_call(x, c("$", "[["), n = 2) && is_symbol(x[[2]], c(".data", ".env"))
}

is_global <- function(env) {
  if (identical(env, globalenv())) {
    return(TRUE)
  }

  # Heuristic for inside pipe
  if (identical(env_names(env), ".") && identical(env_parent(env), globalenv())) {
    return(TRUE)
  }

  FALSE
}

simplify_function_call <- function(x, env, data, j = TRUE) {
  if (inherits(x[[1]], "inline_colwise_function")) {
    dot_var <- data$vars[[attr(x, "position")]]
    out <- replace_dot(attr(x[[1]], "formula")[[2]], sym(dot_var))
    dt_squash(out, env, data, j = j)
  } else {
    name <- fun_name(x[[1]])
    if (is_call(x, "function")) {
      x[[3]] <- dt_squash(x[[3]], env, data, j)
      return(x)
    } else if (is.null(name)) {
      return(x)
    }

    attr(x, "position") <- NULL
    x[[1]] <- name
    dt_squash(x, env, data, j = j)
  }
}

replace_dot <- function(call, sym) {
  if (is_symbol(call, ".") || is_symbol(call, ".x")) {
    sym
  } else if (is_call(call)) {
    call[] <- lapply(call, replace_dot, sym)
    call
  } else {
    call
  }
}

has_gforce <- c(
  "min", "max", "mean", "median", "var", "sd", "sum", "prod",
  "first", "last", "head", "tail"
)
dplyr_trans <- c("n", "row_number")

fun_name <- function(fun) {
  pkg_env <- baseenv()

  for (x in has_gforce) {
    if (!env_has(pkg_env, x, inherit = TRUE))
      next

    fun_x <- env_get(pkg_env, x, inherit = TRUE)
    if (identical(fun, fun_x))
      return(sym(x))
  }

  dplyr_env <- pkg_env("dplyr")
  for (x in dplyr_trans) {
    fun_x <- env_get(dplyr_env, x, inherit = TRUE)
    if (identical(fun, fun_x))
      return(sym(x))
  }

  NULL
}

check_one_arg <- function(x) {
  fun <- call_name(x)
  args <- call_args(x)
  if (!has_length(args, 1L)) {
    abort(glue("`{fun}()` expects exactly one argument."))
  }
}

prep_case_match_dot <- function(dot, .x) {
  lhs <- f_lhs(dot)
  if (is.character(lhs) || is.numeric(lhs)) {
    lhs <- call2("==", .x, lhs)
  } else {
    lhs <- call2("%in%", .x, lhs)
  }
  f_lhs(dot) <- lhs
  dot
}
