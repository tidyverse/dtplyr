test_that("unite pastes columns together & removes old col", {
  df <- lazy_dt(data.table(x = "a", y = "b"), "DT")
  step <- unite(df, "z", x:y)
  out <- as.data.table(step)
  expect_equal(names(out), "z")
  expect_equal(out$z, "a_b")
  expect_equal(
    show_query(step),
    expr(setcolorder(copy(DT)[, `:=`(z = paste(x, y, sep = "_"))], !!c("z", "x", "y"))[, .(z)])
  )
})

test_that("unite does not remove new col in case of name clash", {
  df <- lazy_dt(data.table(x = "a", y = "b"), "DT")
  step <- unite(df, x, x:y)
  out <- as.data.table(step)
  expect_equal(names(out), "x")
  expect_equal(out$x, "a_b")
})

test_that("unite preserves grouping", {
  df <- lazy_dt(data.table(g = 1, x = "a"), "DT") %>% group_by(g)
  step <- df %>% unite(x, x)
  expect_equal(dplyr::group_vars(df), dplyr::group_vars(step))
})

test_that("doesn't use `by` for unite step", {
  df <- lazy_dt(data.table(x = "a", y = "b", z = "c"), "DT") %>% group_by(z)
  step <- unite(df, "z", x:y)
  out <- as.data.table(step)
  expect_equal(names(out), "z")
  expect_equal(out$z, "a_b")
  expect_equal(step$groups, "z")
  expect_equal(
    show_query(step),
    expr(copy(DT)[, `:=`(z = paste(x, y, sep = "_"))][, .(z)])
  )
})

test_that("drops grouping when needed", {
  df <- lazy_dt(data.table(g = 1, x = "a"), "DT") %>% group_by(g)
  step <- df %>% unite(gx, g, x)
  rs <- as.data.table(step)
  expect_equal(rs$gx, "1_a")
  expect_equal(dplyr::group_vars(rs), character())
})

test_that("keeps groups when needed", {
  df <- lazy_dt(data.table(x = "x", y = "y"), "DT") %>% group_by(x, y)
  step <- df %>% unite("z", x)
  rs <- as.data.table(step)
  expect_equal(rs$z, "x")
  expect_equal(dplyr::group_vars(step), "y")
})

test_that("empty var spec uses all vars", {
  df <- lazy_dt(data.table(x = "a", y = "b"), "DT")
  expect_equal(collect(unite(df, "z")), tibble(z = "a_b"))
})

test_that("errors on na.rm", {
  df <- lazy_dt(data.table(x = c("a", NA), y = c("b", NA)), "DT")
  expect_snapshot_error(unite(df, "z", x:y, na.rm = TRUE))
})
