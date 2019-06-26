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
    expr(dt1[dt2, on = .(x)])
  )

  expect_equal(
    dt1 %>% right_join(dt2, by = "x") %>% show_query(),
    expr(dt2[dt1, on = .(x)])
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
    expr(dt1[unique(dt1[dt2, which = TRUE, nomatch = 0L, on = .(x)])])
  )
})
