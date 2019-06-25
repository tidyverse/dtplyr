test_that("constructor has sensible defaults", {
  dt <- data.table(x = 1:2, y = 1:2)
  step <- new_step_first(dt)

  expect_s3_class(step, "dtplyr_step_first")
  expect_equal(step$parent, dt)
  expect_equal(step$vars, c("x", "y"))
  expect_equal(step$groups, character())
  expect_match(as.character(step$name), "_DT")
})

test_that("doesn't need copy", {
  dt <- lazy_dt(mtcars)
  expect_false(dt_needs_copy(dt))
})

test_that("dt_call() copies if requested", {
  dt <- lazy_dt(mtcars, name = "DT")

  expect_equal(dt_call(dt, FALSE), quote(DT))
  expect_equal(dt_call(dt, TRUE), quote(copy(DT)))
})
