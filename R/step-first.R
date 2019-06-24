new_step_first <- function(parent, env = caller_env()) {
  stopifnot(is.data.table(parent))

  new_step(parent,
    vars = names(parent),
    groups = character(),
    env = env,
    class = "dtplyr_step_first"
  )
}

dt_call.dtplyr_step_first <- function(x, needs_copy = FALSE) {
  if (needs_copy) {
    quote(copy(`_DT`))
  } else {
    quote(`_DT`)
  }
}

dt_needs_copy.dtplyr_step_first <- function(x) {
  FALSE
}
