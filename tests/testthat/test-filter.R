context("filter")

test_that("filter succeeds even if column called V1 (#615)", {
  dt <- data.table(x = 1:10 ,V1 = 0)
  out <- dt %>% group_by(V1) %>% filter(x > 5)

  expect_equal(nrow(out), 5)
})

test_that("filter_ works (#906)", {
  dt <- data.table::data.table(x = 1:10 ,V1 = 0)
  out <- dt %>% filter_(~x > 5)
  expect_equal(nrow(out), 5)
})

