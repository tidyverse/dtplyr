context("select")

test_that("adds grouping variables", {
  res <- mtcars %>% tbl_dt() %>% group_by(vs) %>% select(mpg)
  expect_named(res, c("vs", "mpg"))
})

test_that("select changes columns in copy of data table", {
  dt <- data.table::data.table(x = 1:4, y = letters[1:4])

  expect_equal(names(select(dt, x, z = y)), c("x", "z"))
  expect_equal(names(dt), c("x", "y"))

  gdt <- dt %>% group_by(x)
  expect_equal(names(select(gdt, x, z = y)), c("x", "z"))
  expect_equal(names(gdt), c("x", "y"))
})
