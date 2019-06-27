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
