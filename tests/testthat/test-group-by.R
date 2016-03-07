context("group_by")

test_that("original data table not modified by grouping", {
  dt <- data.table(x = 5:1)
  dt2 <- group_by(dt, x)
  dt2$y <- 1:5

  expect_equal(dt$x, 5:1)
  expect_equal(dt$y, NULL)
})

test_that("group_by with add = TRUE adds groups", {
  dt <- data.table(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

  add_groups1 <- function(tbl) groups(group_by(tbl, x, y, add = TRUE))
  add_groups2 <- function(tbl) groups(group_by(group_by(tbl, x, add = TRUE), y,
    add = TRUE))

  expect_equal(add_groups1(dt), list(quote(x), quote(y)))
  expect_equal(add_groups2(dt), list(quote(x), quote(y)))
})

test_that("data.table invalid .selfref issue (dplyr#475)", {
  dt <- data.table(x = 1:5, y = 6:10)
  expect_warning((dt %>% group_by(x))[, z := 2L], NA)
  dt <- data.table(x = 1:5, y = 6:10)
  expect_warning((dt %>% group_by(x) %>% summarise(z = y ^ 2))[, foo := 1L], NA)
})


test_that("constructors drops groups", {
  dt <- lahman_dt() %>% tbl("Batting") %>% group_by(playerID)
  expect_equal(groups(tbl_dt(dt)), NULL)
})
