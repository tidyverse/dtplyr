context("do")

# Ungrouped ---------------------------------------------------------------

test_that("ungrouped data table with unnamed argument returns data table", {
  out <- data.table(mtcars) %>% do(head(.))
  expect_is(out, "data.table")
  expect_equal(dim(out), c(6, 11))
})

test_that("ungrouped tbl_dt with unnamed argument returns tbl_dt", {
  out <- tbl_dt(mtcars) %>% do(head(.))
  expect_is(out, "tbl_dt", "data.table")
  expect_is(out, "daata.table")
  expect_equal(dim(out), c(6, 11))
})

test_that("ungrouped tbl_dt with named argument returns tbl_dt", {
  out <- data.table(mtcars) %>% do(x = 1, y = 2:10)
  expect_is(out, "data.table")
  expect_equal(out$x, list(1))
  expect_equal(out$y, list(2:10))
})

test_that("ungrouped data table with named argument returns data table", {
  out <- tbl_dt(mtcars) %>% do(x = 1, y = 2:10)
  expect_is(out, "tbl_dt")
  expect_is(out, "data.table")
  expect_equal(out$x, list(1))
  expect_equal(out$y, list(2:10))
})

# Grouped -----------------------------------------------------------------

grp <- data.table(
  g = c(1, 2, 2, 3, 3, 3),
  x = 1:6,
  y = 6:1
) %>% tbl_dt() %>% group_by(g)

test_that("named argument become list columns", {
  out <- grp %>% do(nrow = nrow(.), ncol = ncol(.))
  expect_equal(out$nrow, list(1, 2, 3))
  # doesn't including grouping columns
  expect_equal(out$ncol, list(3, 3, 3))
})

test_that("unnamed results bound together by row", {
  first <- grp %>% do(head(., 1))

  expect_equal(nrow(first), 3)
  expect_equal(first$g, 1:3)
  expect_equal(first$x, c(1, 2, 4))
})

test_that("grouped_dt do evaluates args in correct env", {
  a <- 10
  f <- function(a) {
    grp %>% do(a = a)
  }

  expect_warning(x <- f(20)$a)
  expect_equal(x, list(20, 20, 20))
})

test_that("grouped_dt passes all columns", {
  out <- mtcars %>%
    tbl_dt() %>%
    select(mpg, cyl) %>%
    group_by(cyl) %>%
    do(n = names(.))

  expect_equal(out$n[[1]], c("mpg", "cyl"))
})
