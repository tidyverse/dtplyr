new_step_first <- function(parent, name = NULL, env = caller_env()) {
  stopifnot(is.data.table(parent))

  if (is.null(name)) {
    name <- unique_name()
  }

  new_step(parent,
    vars = names(parent),
    groups = character(),
    name = sym(name),
    env = env,
    class = "dtplyr_step_first"
  )
}

dt_call.dtplyr_step_first <- function(x, needs_copy = FALSE) {
  if (needs_copy) {
    expr(copy(!!x$name))
  } else {
    x$name
  }
}

dt_needs_copy.dtplyr_step_first <- function(x) {
  FALSE
}


unique_name <- local({
  i <- 0
  function() {
    i <<- i + 1
    paste0("_DT", i)
  }
})
