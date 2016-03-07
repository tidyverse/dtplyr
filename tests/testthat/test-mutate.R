context("mutate")

test_that("mutate modifies same column repeatedly (#243)", {
  dt <- data.table::data.table(x = 1)
  expect_equal(mutate(dt, x = x + 1, x = x + 1)$x, 3)
})
