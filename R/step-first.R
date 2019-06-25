#' Create a "lazy" data.table
#'
#' A lazy data.table lazy captures the intent of dplyr verbs, only actually
#' performing computation when requested (with [connect()], [as.data.frame()],
#' [data.table::as.data.table()], or [tibble::as_tibble()]). This allows dtplyr
#' to convert dplyr verbs into as few data.table expressions as possible, which
#' leads to a high performance translation.
#'
#' @param x A data table (or something can can be coerced to a data table)
#' @param name Optionally, supply a name to be used in generated expressions.
#'   For expert use only.
#' @export
#' @examples
#' library(dplyr)
#'
#' mtcars2 <- lazy_dt(mtcars)
#' mtcars2
#' mtcars2 %>% select(mpg:cyl)
#' mtcars2 %>% select(x = mpg, y = cyl)
#' mtcars2 %>% filter(x == 1) %>% select(mpg)
#' mtcars2 %>% select(mpg) %>% filter(x == 1)
#' mtcars2 %>% mutate(x2 = x * 2, x4 = x2 * 2)
#' mtcars2 %>% transmute(x2 = x * 2, x4 = x2 * 2)
#' mtcars2 %>% filter(x == 1) %>% mutate(x2 = x * 2)
#'
#' by_cyl <- mtcars2 %>% group_by(cyl)
#' by_cyl %>% summarise(x = mean(x))
#' by_cyl %>% group_by(mpg) %>% mutate(x = mean(x))
#' by_cyl %>% filter(mpg < mean(mpg)) %>% summarise(hp = mean(hp))
lazy_dt <- function(x, name = NULL) {
  new_step_first(as.data.table(x), name = name)
}

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
