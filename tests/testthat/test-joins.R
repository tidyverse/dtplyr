context("joins")


test_that("joining data tables yields a data table (#470)", {
  a <- data.table(x = c(1, 1, 2, 3), y = 1:4)
  b <- data.table(x = c(1, 2, 2, 4), z = 1:4)

  out <- left_join(a, b, "x")
  expect_is(out, "data.table")
  out <- semi_join(a, b, "x")
  expect_is(out, "data.table")
})

test_that("joining data tables does not modify them (#659)", {
  a <- data.table(x = c(1, 1, 2, 3), y = 4:1)
  b <- data.table(x = c(1, 2, 2, 4), z = 1:4)

  test_join <- function(join_fun, ak, bk) {
    data.table::setkeyv(a, ak)
    data.table::setkeyv(b, bk)
    ac <- data.table::copy(a)
    bc <- data.table::copy(b)

    join_fun(a, b, "x")
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
