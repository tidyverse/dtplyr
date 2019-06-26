test_that("constructor has sensible defaults", {
  dt <- data.table(x = 1:2, y = 1:2)
  step <- step_first(dt)

  expect_s3_class(step, "dtplyr_step_first")
  expect_equal(step$parent, dt)
  expect_equal(step$vars, c("x", "y"))
  expect_equal(step$groups, character())
  expect_match(as.character(step$name), "_DT")
})

test_that("doesn't need copy", {
  dt <- lazy_dt(mtcars)
  expect_false(dt$needs_copy)
})

test_that("dt_call() copies if requested", {
  dt <- lazy_dt(mtcars, name = "DT")

  expect_equal(dt_call(dt, FALSE), quote(DT))
  expect_equal(dt_call(dt, TRUE), quote(copy(DT)))
})

test_that("lazy_dt doesn't copy input", {
  dt <- data.table(x = 1)
  lz <- lazy_dt(dt)

  expect_equal(address(dt), address(lz$parent))
})
