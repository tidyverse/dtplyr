test_that(".before and .after relocate individual cols", {
  dt <- lazy_dt(data.table(x = 1, y = 1), "DT")

  expect_equal(
    dt %>% relocate(x, .after = y) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("y", "x")))
  )
  expect_equal(
    dt %>% relocate(y, .before = x) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("y", "x")))
  )
})

test_that("can move blocks of variables", {
  dt <- lazy_dt(data.table(x = 1, a = 1, y = 1, b = 1), "DT")

  expect_equal(
    dt %>% relocate(y, b, .before = a) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("x", "y", "b", "a")))
  )
  expect_equal(
    dt %>% relocate(any_of(c("y", "b")), .before = a) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("x", "y", "b", "a")))
  )
})

test_that("All columns move before (after) columns in .before (.after)", {
  dt <- lazy_dt(data.table(x = 1, a = 1, y = 1, b = 1), "DT")

  expect_equal(
    dt %>% relocate(y, b, .before = c(x, a)) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("y", "b", "x", "a")))
  )
  expect_equal(
    dt %>% relocate(x, a, .after = c(y, b)) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("y", "b", "x", "a")))
  )
})

test_that("extra variables in .before/.after unaffected", {
  dt <- lazy_dt(data.table(a = 1, b = 1, c = 1, d = 1, e = 1), "DT")

  expect_equal(
    dt %>% relocate(b, .after = c(a, c, e)) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("a", "c", "d", "e", "b")))
  )
  expect_equal(
    dt %>% relocate(e, .before = c(b, d)) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("a", "e", "b", "c", "d")))
  )
})

test_that("no .before/.after moves to front", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% relocate(y) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("y", "x")))
  )
})

test_that("can only supply one of .before and .after", {
  dt <- lazy_dt(data.table(x = 1, y = 1), "DT")

  expect_error(relocate(dt, y, .before = x, .after = x), "only one")
})

test_that("relocate() respects order specified by ...", {
  dt <- lazy_dt(data.table(a = 1, x = 1, b = 1, z = 1, y = 1), "DT")

  expect_equal(
    dt %>% relocate(x, y, z, .before = x) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("a", "x", "y", "z", "b")))
  )
  expect_equal(
    dt %>% relocate(x, y, z, .after = last_col()) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("a", "b", "x", "y", "z")))
  )
  expect_equal(
    dt %>% relocate(x, a, z) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("x", "a", "z", "b", "y")))
  )
})

test_that("relocate() only not alter grouping", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% group_by(x, y) %>% relocate(y, .before = x) %>% .$groups,
    c("x", "y")
  )
})
