context("Distinct")

dt <- data.table(
  x = c(1, 1, 1, 1),
  y = c(1, 1, 2, 2),
  z = c(1, 1, 2, 2)
)

test_that("distinct removes duplicates (data.table)", {
  res <- distinct(dt, x)
  expect_is(res, "data.table")
  expect_equal(nrow(res), 1)
})

test_that("distinct removes duplicates (tbl_dt)", {
  res <- distinct(tbl_dt(dt), x)
  expect_is(res, c("tbl_dt", "data.table"))
  expect_equal(nrow(res), 1)
})

test_that("grouped_by uses grouping vars & preserves groups", {
  res <- dt %>% group_by(x) %>% distinct(y)

  expect_is(res, "grouped_dt")
  expect_equal(res$x, c(1, 1))
  expect_equal(res$y, c(1, 2))
})

test_that("distinct works when key is set", {
  dt <- copy(dt)
  setkey(dt, x)

  res <- distinct(dt, x)
  expect_equal(nrow(res), 1)
})


context("Distinct (tests from dplyr):")

test_that("distinct equivalent to local unique when keep_all is TRUE", {
  df <- data.table(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )

  expect_equal(distinct(df), unique(df))
})

test_that("distinct for single column works as expected (#1937)", {
  df <- data.table(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )

  expect_equal(distinct(df, x, .keep_all = FALSE), unique(df[, .(x)]))
  expect_equal(distinct(df, y, .keep_all = FALSE), unique(df[, .(y)]))
})

test_that("distinct works for 0-sized columns (#1437)", {
  df <- tibble(x = 1:10) %>% select(-x)
  ddf <- distinct(df)
  expect_equal(ncol(ddf), 0L)
})

test_that("if no variables specified, uses all", {
  df <- tibble(x = c(1, 1), y = c(2, 2))
  expect_equal(distinct(df), tibble(x = 1, y = 2))
})

test_that("distinct keeps only specified cols", {
  df <- tibble(x = c(1, 1, 1), y = c(1, 1, 1))
  expect_equal(df %>% distinct(x), tibble(x = 1))
})

test_that("unless .keep_all = TRUE", {
  df <- tibble(x = c(1, 1, 1), y = 3:1)

  expect_equal(df %>% distinct(x), tibble(x = 1))
  expect_equal(df %>% distinct(x, .keep_all = TRUE), tibble(x = 1, y = 3L))
})

test_that("distinct doesn't duplicate columns", {
  skip_if_dtplyr()

  df <- data.table(a = 1:3, b = 4:6)

  expect_named(df %>% distinct(a, a), "a")
  expect_named(df %>% group_by(a) %>% distinct(a), "a")
})

test_that("grouped distinct always includes group cols", {
  skip_if_dtplyr()

  df <- data.table(g = c(1, 2), x = c(1, 2))

  out <- df %>% group_by(g) %>% distinct(x)
  expect_equal(df, out)
})

test_that("empty grouped distinct equivalent to empty ungrouped", {
  df <- data.table(g = c(1, 2), x = c(1, 2))

  df1 <- df %>% distinct() %>% group_by(g)
  df2 <- df %>% group_by(g) %>% distinct()

  expect_equal(df1, df2)
})

test_that("distinct on a new, mutated variable is equivalent to mutate followed by distinct", {
  skip_if_dtplyr()
  df <- data.table(g = c(1, 2), x = c(1, 2))

  df1 <- df %>% distinct(aa = g * 2)
  df2 <- df %>% mutate(aa = g * 2) %>% distinct(aa)

  expect_equal(df1, df2)
})


