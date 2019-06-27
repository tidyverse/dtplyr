test_that("tbl metadata as expected", {
  dt <- lazy_dt(data.table(x = c(1, 1, 1, 2, 2, 3)), "DT")

  expect_equal(dim(dt), c(NA, 1))
  expect_equal(tbl_vars(dt), "x")
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

  expect_s3_class(collect(dt), "data.table")
})

# evaluation environment --------------------------------------------------

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
