test_that("`reframe()` allows summaries", {
  df <- lazy_dt(tibble(g = c(1, 1, 1, 2, 2), x = 1:5))

  expect_identical(
    collect(reframe(df, x = mean(x))),
    tibble(x = 3)
  )
  expect_identical(
    collect(reframe(df, x = mean(x), .by = g)),
    tibble(g = c(1, 2), x = c(2, 4.5))
  )
})

test_that("`reframe()` allows size 0 results", {
  df <- lazy_dt(tibble(g = c(1, 1, 1, 2, 2), x = 1:5))
  gdf <- group_by(df, g)

  expect_identical(
    collect(reframe(df, x = which(x > 5))),
    tibble(x = integer())
  )
  expect_identical(
    collect(reframe(df, x = which(x > 5), .by = g)),
    tibble(g = double(), x = integer())
  )
  expect_identical(
    collect(reframe(gdf, x = which(x > 5))),
    tibble(g = double(), x = integer())
  )
})

test_that("`reframe()` allows size >1 results", {
  df <- lazy_dt(tibble(g = c(1, 1, 1, 2, 2), x = 1:5))
  gdf <- group_by(df, g)

  expect_identical(
    collect(reframe(df, x = which(x > 2))),
    tibble(x = 3:5)
  )
  expect_identical(
    collect(reframe(df, x = which(x > 2), .by = g)),
    tibble(g = c(1, 2, 2), x = c(3L, 1L, 2L))
  )
  expect_identical(
    collect(reframe(gdf, x = which(x > 2))),
    tibble(g = c(1, 2, 2), x = c(3L, 1L, 2L))
  )
})

test_that("`reframe()` ungroups output", {
  df <- lazy_dt(tibble(g = c(1, 1, 1, 2, 2), x = 1:5))
  gdf <- group_by(df, g, x)
  res <- reframe(gdf, row_num = row_number())

  expect_true(length(group_vars(res)) == 0)
})
