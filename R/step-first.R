new_step_first <- function(parent, env = caller_env()) {
  stopifnot(is.data.table(parent))

  new_step(parent,
    vars = names(parent),
    groups = character(),
    needs_copy = FALSE,
    env = env,
    class = "dtplyr_step_first"
  )
}

dt_call.dtplyr_step_first <- function(x) {
  quote(`_DT`)
}
