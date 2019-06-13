context("joins")


test_that("joining data tables returns data tables (#470) and does not modify them (#659)", {
  x <- data.table(x = c(1, 1, 2, 3), y = 4:1)
  y <- data.table(x = c(1, 2, 2, 4), z = 1:4)

  join_funs <- list(left_join, semi_join, anti_join)
  for (join_fun in join_funs) {
    xprime <- data.table::copy(x)
    yprime <- data.table::copy(y)

    out <- join_fun(xprime, yprime, "x")
    expect_s3_class(out, "data.table")
    expect_equal(xprime, x)
    expect_equal(yprime, y)
  }
})

test_that("joining data tables returns same result as dplyr", {
  a_dt <- data.table(x = c(1, 1, 2, 3), y = 4:1)
  b_dt <- data.table(x = c(1, 2, 2, 4), z = 1:4)

  a_df <- as_tibble(a_dt)
  b_df <- as_tibble(b_dt)

  test_join <- function(join_fun) {
    out <- join_fun(a_dt, b_dt, "x")
    out_dplyr <- tbl_dt(join_fun(a_df, b_df, "x"))
    expect_equal(out, out_dplyr)
  }

  test_join(left_join)
  test_join(semi_join)
  test_join(right_join)
  test_join(full_join)
  test_join(inner_join)
  test_join(anti_join)
})


test_that("changing suffixes works as in dplyr", {
  a_dt <- data.table(x = c(1, 1, 2, 3), y = 4:1)
  b_dt <- data.table(x = c(1, 2, 2, 4), z = 1:4)

  a_df <- as_tibble(a_dt)
  b_df <- as_tibble(b_dt)

  test_join <- function(join_fun) {
    out <- join_fun(a_dt, b_dt, "x", suffix = c("left", "right"))
    out_dplyr <- tbl_dt(join_fun(a_df, b_df, "x", suffix = c("left", "right")))
    expect_equal(out, out_dplyr)
  }

  test_join(left_join)
  test_join(right_join)
  test_join(full_join)
  test_join(inner_join)
})
