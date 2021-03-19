test_that("complete with no variables returns data as is", {
  mtcars_dt <- lazy_dt(mtcars, "DT")
  expect_equal(complete(mtcars_dt), mtcars_dt)
})

test_that("basic invocation works", {
  tbl <- tibble(x = 1:2, y = 1:2, z = 3:4)
  dt <- lazy_dt(tbl, "DT")
  out <- dt %>% complete(x, y) %>% collect()

  expect_equal(nrow(out), 4)
  expect_equal(out$z, c(3, NA, NA, 4))
})

test_that("empty expansion returns original", {
  tbl <- tibble(x = character())
  dt <- lazy_dt(tbl, "DT")
  out <- dt %>% complete(y = NULL) %>% collect()
  expect_equal(out, tbl)

  tbl <- tibble(x = 1:4)
  dt <- lazy_dt(tbl, "DT")
  out <- dt %>% complete(y = NULL) %>% collect()
  expect_equal(out, tbl)
})
