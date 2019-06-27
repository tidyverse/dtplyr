test_that("group metadata", {
  dt <- lazy_dt(data.table(x = c(1, 1, 1, 2, 2, 3)))
  expect_equal(group_vars(dt), character())
  expect_equal(groups(dt), list())
  expect_equal(group_size(dt), 6)
  expect_equal(n_groups(dt), 1)

  gt <- group_by(dt, x)
  expect_equal(group_vars(gt), c("x"))
  expect_equal(groups(gt), syms("x"))
  expect_equal(group_size(gt), c(3, 2, 1))
  expect_equal(n_groups(gt), 3)
})
