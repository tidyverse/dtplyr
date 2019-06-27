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
        eval(x, env)
      } else {
        x
      }
    }
  } else if (is_quosure(x)) {
    dt_squash(get_expr(x), get_env(x), vars = vars)
  } else if (is_call(x)) {
    x[-1] <- lapply(x[-1], dt_squash, vars = vars, env = env)
    x
  } else {
    abort("Invalid input")
  }
}
