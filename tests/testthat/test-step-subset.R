test_that("construtor has sensible defaults", {
  first <- step_first(data.table(x = 1), "DT")
  step <- step_subset(first)

  expect_s3_class(step, "dtplyr_step_subset")
  expect_equal(step$parent, first)
  expect_equal(step$vars, "x")
  expect_equal(step$groups, character())
  expect_equal(step$i, NULL)
  expect_equal(step$j, NULL)
})

test_that("generates expected calls", {
  first <- lazy_dt(data.table(x = 1), "DT")

  ungrouped <- step_subset(first, i = quote(i), j = quote(j))
  expect_equal(dt_call(ungrouped), expr(DT[i, j]))

  with_i <- step_subset(first, i = quote(i), j = quote(j), groups = "x")
  expect_equal(dt_call(with_i), expr(DT[i, j, keyby = .(x)]))

  without_i <- step_subset(first, j = quote(j), groups = "x")
  expect_equal(dt_call(without_i), expr(DT[, j, keyby = .(x)]))
})
