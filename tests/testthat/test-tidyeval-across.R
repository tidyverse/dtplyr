test_that("across() translates NULL", {
  expect_equal(
    capture_across(letters, across(a:b)),
    list(expr(a), expr(b))
  )
})

test_that("across() translates character vectors", {
  expect_equal(
    capture_across(letters, across(a:b, "log")),
    exprs(a = log(a), b = log(b))
  )

  expect_equal(
    capture_across(letters, across(a:b, "log", base = 2)),
    exprs(a = log(a, base = 2), b = log(b, base = 2))
  )

  expect_equal(
    capture_across(letters, across(a, c("log", "exp"))),
    exprs(a_log = log(a), a_exp = exp(a))
  )
})

test_that("across() translates functions", {
  expect_equal(
    capture_across(letters, across(a:b, log)),
    exprs(a = log(a), b = log(b))
  )

  expect_equal(
    capture_across(letters, across(a:b, log, base = 2)),
    exprs(a = log(a, base = 2), b = log(b, base = 2))
  )

  expect_equal(
    capture_across(letters, across(a, list(log, exp))),
    exprs(a_log = log(a), a_exp = exp(a))
  )
})

test_that("across() translates formulas", {
  expect_equal(
    capture_across(letters, across(a:b, ~ log(.x))),
    exprs(a = log(a), b = log(b))
  )

  # and recursively translates
  expect_equal(
    capture_across(letters, across(a, ~ .x / n())),
    exprs(a = a / .N)
  )

  expect_equal(
    capture_across(letters, across(a:b, list(~log(.x)))),
    exprs(a = log(a), b = log(b))
  )
})

test_that("across() gives informative errors", {
  expect_snapshot(error = TRUE, {
    capture_across(letters, across(a, 1))
    capture_across(letters, across(a, list(1)))
  })
})
