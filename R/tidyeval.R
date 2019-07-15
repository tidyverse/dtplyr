dt_eval <- function(x) {
  env <- as_environment(dt_sources(x), x$env)
  add_dt_wrappers(env)
  quo <- new_quosure(dt_call(x), env)

  eval_tidy(quo)
}

#' @importFrom data.table frank
add_dt_wrappers <- function(env) {
  # Make sure data.table functions are available so dtplyr still works
  # even when data.table isn't attached
  env$setname <- data.table::setnames
  env$copy <- data.table::copy
  env$setkeyv <- data.table::setkeyv

  invisible()
}

# These functions attempt to simulate tidy eval as much as possible within
# data.table. The goal is to get the majority of real-world code to work,
# without aiming for 100% compliance.

capture_dots <- function(.data, ..., .named = TRUE) {
  dots <- enquos(..., .named = .named)
  dots <- lapply(dots, dt_squash, vars = .data$vars)
  dots
}

capture_dot <- function(.data, x) {
  dt_squash(enquo(x), vars = .data$vars)
}

# squash quosures
dt_squash <- function(x, env, vars) {
  if (is_atomic(x)) {
    x
  } else if (is_symbol(x)) {
    if (identical(x, quote(.))) {
      quote(.SD)
    } else {
      var <- as.character(x)
      if (nchar(x) > 0 && substr(var, 1, 1) == ".") {
        # data table pronouns are bound to NULL
        x
      } else if (!var %in% vars && env_has(env, var, inherit = TRUE)) {
        if (identical(env, globalenv())) {
          # This is slightly dangerous because the variable might be modified
          # between creation and execution, but it seems like a reasonable
          # tradeoff in order to get a more natural translation.
          sym(paste0("..", var))
        } else {
          eval(x, env)
        }
      } else {
        x
      }
    }
  } else if (is_quosure(x)) {
    dt_squash(get_expr(x), get_env(x), vars = vars)
  } else if (is_call(x)) {
    if (is_call(x, "n", n = 0)) {
      quote(.N)
    } else if (is_call(x, "row_number", n = 0)) {
      quote(seq_len(.N))
    } else if (is_call(x, "row_number", n = 1)) {
      arg <- dt_squash(x[[2]], vars = vars, env = env)
      expr(frank(!!arg, ties.method = "first", na.last = "keep"))
    } else if (is.function(x[[1]])) {
      simplify_function_call(x, vars)
    } else {
      x[-1] <- lapply(x[-1], dt_squash, vars = vars, env = env)
      x
    }
  } else {
    abort("Invalid input")
  }
}

simplify_function_call <- function(x, vars) {
  if (inherits(x[[1]], "inline_colwise_function")) {
    dot_var <- vars[[attr(x, "position")]]
    replace_dot(attr(x[[1]], "formula")[[2]], dot_var)
  } else {
    x[[1]] <- fun_name(x[[1]])
    x
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

fun_name <- function(fun) {
  pkg_env <- baseenv()

  for (x in has_gforce) {
    if (!env_has(pkg_env, x, inherit = TRUE))
      next

    fun_x <- env_get(pkg_env, x, inherit = TRUE)
    if (identical(fun, fun_x))
      return(sym(x))
  }

  fun
}
