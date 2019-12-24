test_that("dt_sources captures all tables", {
  dt1 <- lazy_dt(data.frame(x = 1), "dt1")
  dt2 <- lazy_dt(data.frame(x = 2), "dt2")
  dt3 <- lazy_dt(data.frame(x = 3), "dt3")

  out <- dt1 %>% left_join(dt2, by = "x") %>% left_join(dt3, by = "x")
  expect_equal(
    dt_sources(out)[c("dt1", "dt2", "dt3")],
    list(dt1 = dt1$parent, dt2 = dt2$parent, dt3 = dt3$parent)
  )
})

# dplyr verbs -------------------------------------------------------------

test_that("simple usage generates expected translation", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 1, y = 2, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x") %>% show_query(),
    expr(merge(dt1, dt2, all.x = TRUE, all.y = FALSE, by.x = "x", by.y = "x", allow.cartesian = TRUE))
  )

  expect_equal(
    dt1 %>% right_join(dt2, by = "x") %>% show_query(),
    expr(merge(dt2, dt1, all.x = TRUE, all.y = FALSE, by.x = "x", by.y = "x", allow.cartesian = TRUE))
  )

  expect_equal(
    dt1 %>% inner_join(dt2, by = "x") %>% show_query(),
    expr(merge(dt1, dt2, all = FALSE, by.x = "x", by.y = "x", allow.cartesian = TRUE))
  )

  expect_equal(
    dt1 %>% full_join(dt2, by = "x") %>% show_query(),
    expr(merge(dt1, dt2, all = TRUE, by.x = "x", by.y = "x", allow.cartesian = TRUE))
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

test_that("named by converted to by.x and by.y", {
  dt1 <- lazy_dt(data.frame(a1 = 1:3, z = 1), "dt1")
  dt2 <- lazy_dt(data.frame(a2 = 1:3, z = 2), "dt2")

  expect_equal(
    dt1 %>% inner_join(dt2, by = c('a1' = 'a2')) %>% show_query(),
    expr(merge(dt1, dt2, all = FALSE, by.x = "a1", by.y = "a2", allow.cartesian = TRUE))
  )
})

test_that("simple left joins use [", {
  dt1 <- lazy_dt(data.frame(x = 1:2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 2:3, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x") %>% show_query(),
    expr(dt2[dt1, on = .(x), allow.cartesian = TRUE])
  )
  expect_equal(
    dt1 %>% left_join(dt2, by = "x") %>% pull(x),
    dt1 %>% pull(x)
  )
})

test_that("correctly determines vars", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 1, y = 2, b = 4), "dt2")

  expect_setequal(
    dt1 %>% left_join(dt2, by = c("x", "y")) %>% .$vars,
    c("x", "y", "a", "b")
  )
  expect_setequal(
    dt1 %>% left_join(dt2, by = "x") %>% .$vars,
    c("x", "y.x", "y.y", "a", "b")
  )
  expect_setequal(
    dt1 %>% semi_join(dt2, by = "x") %>% .$vars,
    c("x", "y", "a")
  )
})


test_that("can override suffixes", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 1, y = 2, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x", suffix = c("X", "Y")) %>% show_query(),
    expr(merge(
      dt1, dt2,
      all.x = TRUE, all.y = FALSE,
      by.x = "x", by.y = "x",
      allow.cartesian = TRUE,
      suffixes = !!c("X", "Y")
    ))
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
  expect_s3_class(left_join(dt1, x, by = "x", copy = TRUE), "dtplyr_step_subset")
})

test_that("mutates inside joins are copied as needed", {
  dt <- data.table(x = 1)
  lhs <- lazy_dt(dt, "dt1") %>% mutate(y = x + 1)
  rhs <- lazy_dt(dt, "dt2") %>% mutate(z = x + 1)

  collect(inner_join(lhs, rhs, by = "x"))
  expect_named(dt, "x")
})

test_that("performs cartesian joins as needed", {
  x <- lazy_dt(data.frame(x = c(2, 2, 2), y = 1:3))
  y <- lazy_dt(data.frame(x = c(2, 2, 2), z = 1:3))
  out <- collect(left_join(x, y))
  expect_equal(nrow(out), 9)
})
