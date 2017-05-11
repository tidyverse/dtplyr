context("joins")


test_that("joining data tables returns data tables (#470) and does not modify them (#659)", {
  a <- data.table(x = c(1, 1, 2, 3), y = 4:1)
  b <- data.table(x = c(1, 2, 2, 4), z = 1:4)

  test_join <- function(join_fun, ak, bk) {
    data.table::setkeyv(a, ak)
    data.table::setkeyv(b, bk)
    ac <- data.table::copy(a)
    bc <- data.table::copy(b)

    out <- join_fun(a, b, "x")
    expect_is(out, "data.table")
    expect_equal(a, ac)
    expect_equal(b, bc)
  }

  for (ak in names(a)) {
    for (bk in names(b)) {
      test_join(left_join, ak, bk)
      test_join(semi_join, ak, bk)
      test_join(right_join, ak, bk)
      test_join(full_join, ak, bk)
      test_join(inner_join, ak, bk)
      test_join(anti_join, ak, bk)
    }
  }
})


test_that("joining data tables returns same result as dplyr", {
  a_dt <- data.table(x = c(1, 1, 2, 3), y = 4:1)
  b_dt <- data.table(x = c(1, 2, 2, 4), z = 1:4)

  a_df <- as_data_frame(a_dt)
  b_df <- as_data_frame(b_dt)

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

