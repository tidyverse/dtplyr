dt_eval <- function(x) {
  env <- as_environment(dt_sources(x), x$env)
  add_dt_wrappers(env)
  quo <- new_quosure(dt_call(x), env)

  eval_tidy(quo)
}

#' @importFrom data.table frank fifelse fcoalesce
add_dt_wrappers <- function(env) {
  # Make sure data.table functions are available so dtplyr still works
  # even when data.table isn't attached
  env$setnames <- data.table::setnames
  env$copy <- data.table::copy
  env$setkeyv <- data.table::setkeyv

  invisible()
}

# These functions attempt to simulate tidy eval as much as possible within
# data.table. The goal is to get the majority of real-world code to work,
# without aiming for 100% compliance.

capture_dots <- function(.data, ..., .j = TRUE) {
  dots <- enquos(..., .named = .j)
  dots <- lapply(dots, dt_squash, vars = .data$vars, j = .j)
  dots
}

capture_dot <- function(.data, x, j = TRUE) {
  dt_squash(enquo(x), vars = .data$vars, j = j)
}

# squash quosures
dt_squash <- function(x, env, vars, j = TRUE) {
  if (is_atomic(x) || is_null(x)) {
    x
  } else if (is_symbol(x)) {
    if (identical(x, quote(.))) {
      quote(.SD)
    } else {
      var <- as.character(x)

      if (var %in% c("T", "F")) {
        as.logical(var)
      } else if (nchar(x) > 0 && substr(var, 1, 1) == ".") {
        # data table pronouns are bound to NULL
        x
      } else if (!var %in% vars && env_has(env, var, inherit = TRUE)) {
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
    dt_squash(get_expr(x), get_env(x), vars = vars, j = j)
  } else if (is_call(x)) {
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
    } else if (is_call(x, "n", n = 0)) {
      quote(.N)
    } else if (is_call(x, "row_number", n = 0)) {
      quote(seq_len(.N))
    } else if (is_call(x, "row_number", n = 1)) {
      arg <- dt_squash(x[[2]], vars = vars, env = env, j = j)
      expr(frank(!!arg, ties.method = "first", na.last = "keep"))
    } else if (is_call(x, "if_else")) {
      x[[1L]] <- quote(fifelse)
      x
    } else if (is_call(x, 'coalesce')) {
      x[[1L]] <- quote(fcoalesce)
      x
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
    } else if (is.function(x[[1]]) || is_call(x, "function")) {
      simplify_function_call(x, env, vars = vars, j = j)
    } else {
      x[-1] <- lapply(x[-1], dt_squash, vars = vars, env = env, j = j)
      x
    }
  } else {
    abort("Invalid input")
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

simplify_function_call <- function(x, env, vars, j = TRUE) {
  if (inherits(x[[1]], "inline_colwise_function")) {
    dot_var <- vars[[attr(x, "position")]]
    out <- replace_dot(attr(x[[1]], "formula")[[2]], dot_var)
    dt_squash(out, env, vars = vars, j = j)
  } else {
    name <- fun_name(x[[1]])
    if (is.null(name)) {
      return(x)
    }

    attr(x, "position") <- NULL
    x[[1]] <- name
    dt_squash(x, env, vars = vars, j = j)
  }
}

replace_dot <- function(call, var) {
  if (is_symbol(call, ".")) {
    sym(var)
  } else if (is_call(call)) {
    call[] <- lapply(call, replace_dot, var = var)
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
