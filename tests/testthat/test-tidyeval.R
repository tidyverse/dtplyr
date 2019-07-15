test_that("simple expressions left as is", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))

  expect_equal(capture_dot(dt, 10), 10)
  expect_equal(capture_dot(dt, x), quote(x))
  expect_equal(capture_dot(dt, x + y), quote(x + y))
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
})

test_that("using environment of inlined quosures", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))

  n <- 10
  quo <- new_quosure(quote(x + n), env(n = 20))

  expect_equal(capture_dot(dt, f(!!quo)), quote(f(x + 20)))
})

test_that(". gets converted to .SD", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))

  expect_equal(capture_dot(dt, .), quote(.SD))
  expect_equal(capture_dot(dt, .SD), quote(.SD))
})

test_that("can process many expressions in one go", {
  dt <- lazy_dt(data.frame(x = 1:10, y = 1:10))
  n <- 10
  dots <- capture_dots(dt, x = x + n, y = y)
  expect_named(dots, c("x", "y"))
  expect_equal(dots$x, quote(x + 10))
})

# evaluation --------------------------------------------------------------

test_that("can access functions in local env", {
  dt <- lazy_dt(data.frame(g = c(1, 1, 2), x = 1:3))
  f <- function(x) 100

  expect_equal(dt %>% summarise(n = f()) %>% pull(), 100)
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
})

# fun_name ----------------------------------------------------------------

test_that("finds name of functions with GForce implementations", {
  expect_equal(fun_name(mean), expr(mean))

  # unless overridden
  mean <- function() {}
  expect_equal(fun_name(mean), mean)
})

