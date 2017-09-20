context("slice")

test_that("slicing data.table yields same output as slicing data.frame ", {
  tbls <- list(mtcars, mtcars %>% tbl_dt())
  compare_tbls(tbls, function(x) x %>% group_by(cyl) %>% slice(c(1, 3)))
})

test_that("slicing data table preserves input class", {
  mtcars_dt <- mtcars %>% data.table()

  expect_is(mtcars_dt %>% slice(1), "data.table")
  expect_is(mtcars_dt %>% tbl_dt() %>% slice(1), "tbl_dt")
  expect_is(mtcars_dt %>% group_by(cyl) %>% slice(1), "grouped_dt")
})

context("slice (tests from dplyr)")
 
mtcars_dt <- as.data.table(mtcars)

test_that("slice handles numeric input (#226)", {
  skip_if_dtplyr()

  g <- mtcars_dt %>% group_by(cyl)
  res <- g %>% slice(1)
  expect_equal(nrow(res), 3)
  expect_equal(res, g %>% filter(row_number() == 1L))

  expect_equal(
    mtcars_dt %>% slice(1),
    mtcars_dt %>% filter(row_number() == 1L)
  )
})

test_that("slice silently ignores out of range values (#226)", {
  skip_if_dtplyr()

  expect_equal(slice(mtcars_dt, c(2, 100)), slice(mtcars_dt, 2))

  g <- group_by(mtcars_dt, cyl)
  expect_equal(slice(g, c(2, 100)), slice(g, 2))

})

test_that("slice works with 0 args", {
  skip_if_dtplyr()

  expect_equivalent(slice(mtcars_dt), mtcars_dt)
})

test_that("slice works with negative indices", {
  res <- slice(mtcars_dt, -(1:2))
  exp <- tail(mtcars_dt, -2)
  expect_equal(names(res), names(exp))
  for (col in names(res)) {
    expect_equal(res[[col]], exp[[col]])
  }
})

test_that("slice forbids positive and negative together", {
  expect_error(
    mtcars_dt %>% slice(c(-1, 2)),
    "Cannot mix positives and negatives.",
    fixed = TRUE
  )
})

test_that("slice works with grouped data", {
  skip_if_dtplyr()

  g <- group_by(mtcars_dt, cyl)

  res <- slice(g, 1:2)
  exp <- filter(g, row_number() < 3)
  expect_equal(res, exp)

  res <- slice(g, -(1:2))
  exp <- filter(g, row_number() >= 3)
  expect_equal(res, exp)

})

test_that("slice gives correct rows (#649)", {
  a <- data_frame(value = paste0("row", 1:10))
  expect_equal(slice(a, 1:3)$value, paste0("row", 1:3))
  expect_equal(slice(a, c(4, 6, 9))$value, paste0("row", c(4, 6, 9)))

  a <- data_frame(
    value = paste0("row", 1:10),
    group = rep(1:2, each = 5)
  ) %>%
    group_by(group)

  expect_equal(slice(a, 1:3)$value, paste0("row", c(1:3, 6:8)))
  expect_equal(slice(a, c(2, 4))$value, paste0("row", c(2, 4, 7, 9)))
})

test_that("slice handles NA (#1235)", {
  df <- data_frame(x = 1:3)
  expect_equal(nrow(slice(df, NA_integer_)), 0L)
  expect_equal(nrow(slice(df, c(1L, NA_integer_))), 1L)
  expect_equal(nrow(slice(df, c(-1L, NA_integer_))), 2L)

  df <- data_frame(x = 1:4, g = rep(1:2, 2)) %>% group_by(g)
  expect_equal(nrow(slice(df, NA)), 0L)
  expect_equal(nrow(slice(df, c(1, NA))), 2)
  expect_equal(nrow(slice(df, c(-1, NA))), 2)

})

test_that("slice handles empty data frames (#1219)", {
  df <- data.frame(x = numeric())
  res <- df %>% slice(1:3)
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), "x")
})

test_that("slice works fine if n > nrow(df) (#1269)", {
  skip_if_dtplyr()

  slice_res <- mtcars_dt %>% group_by(cyl) %>% slice(8)
  filter_res <- mtcars_dt %>% group_by(cyl) %>% filter(row_number() == 8)
  expect_equal(slice_res, filter_res)
})

test_that("slice strips grouped indices (#1405)", {
  skip_if_dtplyr()

  res <- mtcars_dt %>% group_by(cyl) %>% slice(1) %>% mutate(mpgplus = mpg + 1)
  expect_equal(nrow(res), 3L)
  expect_equal(attr(res, "indices"), as.list(0:2))
})

test_that("slice works with zero-column data frames (#2490)", {
  expect_equal(
    data_frame(a = 1:3) %>% select(-a) %>% slice(1) %>% nrow,
    1L
  )
})

test_that("slice works under gctorture2", {
  skip_if_dtplyr()
  
  x <- data.table(y = 1:10)
  with_gctorture2(999, x2 <- slice(x, 1:10))
  expect_identical(x, x2)
})

test_that("slice correctly computes positive indices from negative indices (#3073)", {
  x <- data.table(y = 1:10)
  # data.table gives a warning here
  expect_identical(suppressWarnings(slice(x, -10:-30)), data.table(y = 1:9))
})