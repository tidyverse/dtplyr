context("test-pivot-wide")

as_dt <- function(...){
  x <- tibble(...)
  x <- as.data.table(x)
  lazy_dt(x)
}

test_that("can pivot all cols to wide", {
  df <- as_dt(key = c("x", "y", "z"), val = 1:3)
  pv <- dt_pivot_wider(df, names_from = key, values_from = val)
  expect_equal(pull(count(pv)), 1)
})

test_that("non-pivoted cols are preserved", {
  df <- as_dt(a = 1, key = c("x", "y"), val = 1:2)
  pv <- dt_pivot_wider(df, names_from = key, values_from = val)

  expect_named(as_tibble(pv), c("a", "x", "y"))
  expect_equal(pull(count(pv)), 1)
})

test_that("implicit missings turn into explicit missings", {
  df <- as_dt(a = 1:2, key = c("x", "y"), val = 1:2)
  pv <- dt_pivot_wider(df, names_from = key, values_from = val)
  pv <- as_tibble(pv)
  expect_equal(pv$a, c(1, 2))
  expect_equal(pv$x, c(1, NA))
  expect_equal(pv$y, c(NA, 2))
})

test_that("warn when overwriting existing column", {
  df <- as_dt(
    a = c(1, 1),
    key = c("a", "b"),
    val = c(1, 2)
  )
  expect_error(
    as_tibble(dt_pivot_wider(df, names_from = key, values_from = val))
  )
})

test_that("names_prefix fails", {
  df <- as_dt(key = c("x", "y", "z"), val = 1:3)
  expect_error(
    dt_pivot_wider(df, names_from = key, values_from = val, names_prefix = "test"),
    "not supported"
  )
})

test_that("grouping is preserved", {
  df <- as_dt(g = 1, k = "x", v = 2)
  out <- df %>%
    dplyr::group_by(g) %>%
    dt_pivot_wider(names_from = k, values_from = v)
  out
  expect_equal(dplyr::group_vars(out), "g")
})

# keys ---------------------------------------------------------

test_that("can override default keys", {
  df <- tribble(
    ~row, ~name, ~var, ~value,
    1,    "Sam", "age", 10,
    2,    "Sam", "height", 1.5,
    3,    "Bob", "age", 20,
  ) %>%
    as.data.table() %>%
    lazy_dt()
  pv <- dt_pivot_wider(df, id_cols = name, names_from = var, values_from = value)
  expect_equal(nrow(as_tibble(pv)), 2)
})
