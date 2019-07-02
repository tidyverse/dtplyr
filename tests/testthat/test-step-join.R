test_that("dt_sources captures all tables", {
  dt1 <- lazy_dt(data.frame(x = 1), "dt1")
  dt2 <- lazy_dt(data.frame(x = 2), "dt2")
  dt3 <- lazy_dt(data.frame(x = 3), "dt3")

  out <- dt1 %>% left_join(dt2, by = "x") %>% left_join(dt3, by = "x")
  expect_equal(
    dt_sources(out),
    list(dt1 = dt1$parent, dt2 = dt2$parent, dt3 = dt3$parent)
  )
})

# dplyr verbs -------------------------------------------------------------

test_that("simple usage generates expected translation", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 1, y = 2, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x") %>% show_query(),
    expr(merge(dt1, dt2, all.x = TRUE, all.y = FALSE, by = "x"))
  )

  expect_equal(
    dt1 %>% right_join(dt2, by = "x") %>% show_query(),
    expr(merge(dt2, dt1, all.x = TRUE, all.y = FALSE, by = "x"))
  )

  expect_equal(
    dt1 %>% inner_join(dt2, by = "x") %>% show_query(),
    expr(merge(dt1, dt2, all = FALSE, by = "x"))
  )

  expect_equal(
    dt1 %>% full_join(dt2, by = "x") %>% show_query(),
    expr(merge(dt1, dt2, all = TRUE, by = "x"))
  )

  expect_equal(
    dt1 %>% anti_join(dt2, by = "x") %>% show_query(),
    expr(dt1[!dt2, on = .(x)])
  )

  expect_equal(
    dt1 %>% semi_join(dt2, by = "x") %>% show_query(),
    expr(dt1[unique(dt1[dt2, which = TRUE, nomatch = NULL, on = .(x)])])
  )
})

test_that("correctly determines vars", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 1, y = 2, b = 4), "dt2")

  expect_equal(dt1 %>% left_join(dt2, by = "x") %>% .$vars, c("x", "y", "a", "b"))
  expect_equal(dt1 %>% semi_join(dt2, by = "x") %>% .$vars, c("x", "y", "a"))
})


test_that("can override suffixes", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 1, y = 2, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x", suffix = c("X", "Y")) %>% show_query(),
    expr(merge(dt1, dt2, all.x = TRUE, all.y = FALSE, by = "x", suffixes = !!c("X", "Y")))
  )
})

test_that("automatically data.frame converts to lazy_dt", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  df2 <- data.frame(x = 1, y = 2, a = 3)

  out <- left_join(dt1, df2, by = "x")
  expect_s3_class(out, "dtplyr_step_join")
})

test_that("converts other types if requested", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  x <- structure(10, class = "foo")

  expect_error(left_join(dt1, x, by = "x"), "copy")
  expect_s3_class(left_join(dt1, x, by = "x", copy = TRUE), "dtplyr_step_join")
})

test_that("mutates inside joins are copied as needed", {
  dt <- data.table(x = 1)
  lhs <- lazy_dt(dt, "dt1") %>% mutate(y = x + 1)
  rhs <- lazy_dt(dt, "dt2") %>% mutate(z = x + 1)

  collect(inner_join(lhs, rhs, by = "x"))
  expect_named(dt, "x")
})
