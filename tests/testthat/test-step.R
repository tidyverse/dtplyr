test_that("tbl metadata as expected", {
  dt <- lazy_dt(data.table(x = c(1, 1, 1, 2, 2, 3)), "DT")

  expect_equal(dim(dt), c(6, 1))
  expect_equal(as.character(tbl_vars(dt)), "x")
  expect_equal(show_query(dt), expr(DT))
})

test_that("group metadata as expected", {
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

test_that("has useful print method", {
  dt <- lazy_dt(mtcars, "DT")
  expect_known_output(print(dt), test_path("test-step-print.txt"))
})

test_that("can evaluate to any data frame type", {
  dt <- lazy_dt(mtcars, "DT")

  expect_identical(class(as.data.frame(dt)), "data.frame")
  expect_s3_class(as.data.table(dt), "data.table")
  expect_s3_class(as_tibble(dt), "tbl_df")

  expect_s3_class(collect(dt), "tbl_df")
})

test_that("compute returns lazy_dt", {
  dt <- lazy_dt(mtcars, "DT")
  dt <- summarise(dt, n = n())

  dt2 <- compute(dt)
  expect_s3_class(dt2, "dtplyr_step")
  expect_equal(as.character(tbl_vars(dt2)), "n")
})
