test_that("simple expressions left as is", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))

  expect_equal(capture_dot(dt, NULL), NULL)
  expect_equal(capture_dot(dt, 10), 10)
  expect_equal(capture_dot(dt, x), quote(x))
  expect_equal(capture_dot(dt, x + y), quote(x + y))
  expect_equal(capture_dot(dt, x[[1]]), quote(x[[1]]))

  # logicals
  expect_equal(eval(capture_dot(dt, T), globalenv()), TRUE)
  expect_equal(eval(capture_dot(dt, F), globalenv()), FALSE)
  expect_equal(capture_dot(dt, TRUE), TRUE)
  expect_equal(capture_dot(dt, FALSE), FALSE)
})

test_that("existing non-variables get inlined", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))

  n <- 10
  expect_equal(capture_dot(dt, x + n), quote(x + 10))
  expect_equal(capture_dot(dt, x + m), quote(x + m))
})

test_that("unless we're operating in the global environment", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))
  quo <- new_quosure(quote(x + n), globalenv())

  expect_equal(capture_dot(dt, !!quo), quote(x + ..n))
  expect_equal(capture_dot(dt, !!quo, j = FALSE), quote(x + n))
})

test_that("using environment of inlined quosures", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))

  n <- 10
  quo <- new_quosure(quote(x + n), env(n = 20))

  expect_equal(capture_dot(dt, f(!!quo)), quote(f(x + 20)))
  expect_equal(capture_dot(dt, f(!!quo), j = FALSE), quote(f(x + 20)))
})

test_that(". gets converted to .SD", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))

  expect_equal(capture_dot(dt, .), quote(.SD))
  expect_equal(capture_dot(dt, .SD), quote(.SD))
})

test_that("translate context functions", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))
  expect_equal(capture_dot(dt, cur_data()), quote(.SD))
  expect_error(capture_dot(dt, cur_data_all()), "not available")
  expect_equal(capture_dot(dt, cur_group()), quote(.BY))
  expect_equal(capture_dot(dt, cur_group_id()), quote(.GRP))
  expect_equal(capture_dot(dt, cur_group_rows()), quote(.I))
})


test_that("translates if_else()/ifelse()", {
  df <- data.frame(x = 1:5)

  expect_equal(
    capture_dot(df, ifelse(x < 0, 1, 2)),
    expr(fifelse(x < 0, 1, 2))
  )
  expect_equal(
    capture_dot(df, if_else(x < 0, 1, 2)),
    expr(fifelse(x < 0, 1, 2))
  )

  # Handles unusual argument names/order
  expect_equal(
    capture_dot(df, ifelse(x < 0, n = 2, yes = 1)),
    expr(fifelse(x < 0, 1, 2))
  )
  expect_equal(
    capture_dot(df, if_else(x < 0, f = 2, true = 1)),
    expr(fifelse(x < 0, 1, 2))
  )

  # tidyeval works inside if_else, #220
  expect_equal(
    capture_dot(df,  if_else(.data$x < 3, 1, 2)),
    expr(fifelse(x < 3, 1, 2))
  )
})

test_that("translates coalesce()", {
  df <- data.frame(x = 1:5)
  expect_equal(
    capture_dot(df, coalesce(x, 1)),
    expr(fcoalesce(x, 1))
  )
})

test_that("can use local variable with coalesce() and replace_na()", {
  dt <- lazy_dt(data.frame(x = c(1, NA)), "dt")
  n <- 10
  expect_equal(
    capture_dot(dt, coalesce(x, n)),
    expr(fcoalesce(x, 10))
  )
  expect_equal(
    capture_dot(dt, replace_na(x, n)),
    expr(fcoalesce(x, 10))
  )
})

test_that("translates case_when()", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))

  expect_equal(
    capture_dot(dt, case_when(x1 ~ y1, x2 ~ y2, x3 ~ TRUE, TRUE ~ y4)),
    quote(fcase(x1, y1, x2, y2, x3, TRUE, rep(TRUE, .N), y4))
  )

  # can use T for default, #272
  expect_equal(
    capture_dot(dt, case_when(x1 ~ y1, x2 ~ y2, x3 ~ TRUE, T ~ y4)),
    quote(fcase(x1, y1, x2, y2, x3, TRUE, rep(TRUE, .N), y4))
  )

  # translates recursively
  expect_equal(
    capture_dot(dt, case_when(x == 1 ~ n())),
    quote(fcase(x == 1, .N))
  )
})

test_that("translates lag()/lead()", {
  df <- data.frame(x = 1:5, y = 1:5)
  expect_equal(
    capture_dot(df, lag(x)),
    expr(shift(x, type = "lag"))
  )
  expect_equal(
    capture_dot(df, lead(x, 2, default = 3)),
    expr(shift(x, n = 2, fill = 3, type = "lead"))
  )
  # Errors with order_by
  expect_snapshot_error(
    capture_dot(df, lag(x, order_by = y)),
  )
})

test_that("can use local variable with lag()/lead()", {
  dt <- lazy_dt(data.frame(x = c(1, NA)), "dt")
  n <- 10
  expect_equal(
    capture_dot(dt, lag(x, n)),
    expr(shift(x, n = 10, type = "lag"))
  )
})

test_that("can process many expressions in one go", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))
  n <- 10
  dots <- capture_dots(dt, x = x + n, y = y)
  expect_named(dots, c("x", "y"))
  expect_equal(dots$x, quote(x + 10))
})

test_that("can use anonymous functions", {
  dt <- lazy_dt(data.frame(x = 1:2, y = 1))

  expect_equal(
    capture_dot(dt, x = sapply(x, function(x) x)) %>% deparse(),
    "sapply(x, function(x) x)"
  )
})

test_that("can splice a data frame", {
  df <- data.frame(b = rep(2, 3), c = rep(3, 3))
  dots <- capture_dots(df, !!!df)
  expect_equal(dots, as.list(df))
})

# evaluation --------------------------------------------------------------

test_that("can access functions in local env", {
  dt <- lazy_dt(data.frame(g = c(1, 1, 2), x = 1:3))
  f <- function(x) 100

  expect_equal(dt %>% summarise(n = f()) %>% pull(), 100)
})

test_that("can disambiguate using .data and .env", {
  dt <- lazy_dt(data.frame(x = 1))
  x <- 2

  expect_equal(capture_dot(dt, .data$x), quote(x))
  expect_equal(capture_dot(dt, .env$x), quote(..x))

  out <- dt %>% summarise(data = .data$x, env = .env$x) %>% as_tibble()
  expect_equal(out, tibble(data = 1, env = 2))

  var <- "x"
  out <- dt %>% summarise(data = .data[[var]], env = .env[[var]]) %>% collect()
  expect_equal(out, tibble(data = 1, env = 2))
})

test_that("locals are executed before call", {
  dt <- lazy_dt(data.frame(x = 1, y = 2))

  expect_equal(
    dt %>% step_locals(exprs(a = 1, b = 2, c = a + b), "c") %>% dt_eval(),
    3
  )
})

# dplyr verbs -------------------------------------------------------------

test_that("n() is equivalent to .N", {
  dt <- lazy_dt(data.frame(g = c(1, 1, 2), x = 1:3))

  expect_equal(
    dt %>% summarise(n = n()) %>% pull(),
    3L
  )
  expect_equal(
    dt %>% group_by(g) %>% summarise(n = n()) %>% pull(),
    c(2L, 1L)
  )
})

test_that("row_number() is equivalent .I", {
  dt <- lazy_dt(data.frame(g = c(1, 1, 2), x = 1:3))

  expect_equal(
    dt %>% mutate(n = row_number()) %>% pull(),
    1:3L
  )
  expect_equal(
    dt %>% group_by(g) %>% mutate(n = row_number()) %>% pull(),
    c(1:2, 1)
  )
})

test_that("row_number(x) is equivalent to rank", {
  dt <- lazy_dt(data.frame(x = c(10, 30, 20)))
  expect_equal(
    dt %>% mutate(n = row_number(x)) %>% pull(),
    c(1L, 3L, 2L)
  )
})

test_that("scoped verbs produce nice output", {
  dt <- lazy_dt(data.table(x = 1:5), "DT")

  expect_equal(
    dt %>% summarise_all(mean) %>% show_query(),
    expr(DT[, .(x = mean(x))])
  )
  expect_equal(
    dt %>% summarise_all(~ mean(.)) %>% show_query(),
    expr(DT[, .(x = mean(x))])
  )

  expect_equal(
    dt %>% summarise_all(row_number) %>% show_query(),
    expr(DT[, .(x = frank(x, ties.method = "first", na.last = "keep"))])
  )
  expect_equal(
    dt %>% summarise_all(~ n()) %>% show_query(),
    expr(DT[, .(x = .N)])
  )
})

test_that("non-Gforce verbs work", {
  dt <- lazy_dt(data.table(x = 1:2), "DT")
  add <- function(x) sum(x)

  expect_equal(dt %>% summarise_at(vars(x), add) %>% pull(), 3)
  expect_equal(dt %>% mutate_at(vars(x), add) %>% pull(), c(3, 3))
})

test_that("`desc(col)` is translated to `-col` inside arrange", {
  dt <- lazy_dt(data.table(x = c("a", "b")), "DT")
  step <- arrange(dt, desc(x))
  out <- collect(step)

  expect_equal(show_query(step), expr(DT[order(-x)]))
  expect_equal(out$x, c("b", "a"))
})

test_that("desc() checks the number of arguments", {
  expect_snapshot(error = TRUE, capture_dot(df, desc(a, b)))
})

test_that("n_distinct() is translated to uniqueN()", {
  # Works with multiple inputs
  expect_equal(
    dt_squash(expr(n_distinct(c(1, 1, 2), c(1, 2, 1)))),
    expr(uniqueN(data.table(c(1, 1, 2), c(1, 2, 1))))
  )
  # Works with single column selection (in summarise())
  expect_equal(
    dt_squash(expr(n_distinct(x))),
    expr(uniqueN(x))
  )
  dt <- lazy_dt(data.table(x = c("a", "a", "b", NA)), "DT")
  step <- summarise(dt, num = n_distinct(x, na.rm = TRUE))
  out <- collect(step)
  expect_equal(
    show_query(step),
    expr(DT[, .(num = uniqueN(x, na.rm = TRUE))])
  )
  expect_equal(out$num, 2)
})

# fun_name ----------------------------------------------------------------

test_that("finds name of functions with GForce implementations", {
  expect_equal(fun_name(mean), expr(mean))

  # unless overridden
  mean <- function() {}
  expect_equal(fun_name(mean), NULL)
})

