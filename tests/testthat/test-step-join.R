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

test_that("joins captures locals from both parents", {
  dt1 <- lazy_dt(data.frame(x = 1)) %>% mutate(y = 1) %>% compute("D1")
  dt2 <- lazy_dt(data.frame(x = 1)) %>% mutate(z = 1) %>% compute("D2")

  expect_named(left_join(dt1, dt2, by = "x")$locals, c("D1", "D2"))
  expect_named(inner_join(dt1, dt2, by = "x")$locals, c("D1", "D2"))
})

# dplyr verbs -------------------------------------------------------------

test_that("simple usage generates expected translation", {
  ldt1 <- lazy_dt(tibble(x = 1, y = 2, a = 3), "dt1")
  ldt2 <- lazy_dt(tibble(x = 1, y = 22, b = 24), "dt2")

  expect_equal(
    ldt1 %>% left_join(ldt2, by = "x") %>% show_query(),
    expr(
      setnames(
        setcolorder(
          copy(dt2)[dt1, on = .(x), allow.cartesian = TRUE],
          !!c("x", "i.y", "a", "y", "b")
        ),
        !!c("i.y", "y"),
        !!c("y.x", "y.y")
      )
    )
  )

  expect_equal(
    ldt1 %>% right_join(ldt2, by = "x") %>% show_query(),
    expr(
      setnames(
        copy(dt1)[dt2, on = .(x), allow.cartesian = TRUE],
        !!c("y", "i.y"),
        !!c("y.x", "y.y")
      )
    )
  )

  expect_equal(
    ldt1 %>% inner_join(ldt2, by = "x") %>% show_query(),
    expr(
      setnames(
        copy(dt1)[dt2, on = .(x), nomatch = NULL],
        !!c("y", "i.y"),
        !!c("y.x", "y.y")
      )
    )
  )

  expect_equal(
    ldt1 %>% full_join(ldt2, by = "x") %>% show_query(),
    expr(merge(dt1, dt2, all = TRUE, by.x = "x", by.y = "x", allow.cartesian = TRUE))
  )

  expect_equal(
    ldt1 %>% anti_join(ldt2, by = "x") %>% show_query(),
    expr(dt1[!dt2, on = .(x)])
  )

  expect_equal(
    ldt1 %>% semi_join(ldt2, by = "x") %>% show_query(),
    expr(dt1[unique(dt1[dt2, which = TRUE, nomatch = NULL, on = .(x)])])
  )
})

test_that("named by converted to by.x and by.y", {
  dt1 <- lazy_dt(data.frame(a1 = 1:3, z = 1), "dt1")
  dt2 <- lazy_dt(data.frame(a2 = 1:3, z = 2), "dt2")

  out_inner <- inner_join(dt1, dt2, by = c('a1' = 'a2'))
  expect_equal(
    out_inner %>% show_query(),
    expr(setnames(copy(dt1)[dt2, on = .(a1 = a2), nomatch = NULL], !!c("z", "i.z"), !!c("z.x", "z.y")))
  )
  expect_setequal(tbl_vars(out_inner), c("a1", "z.x", "z.y"))

  out_left <- left_join(dt1, dt2, by = c('a1' = 'a2'))
  expect_equal(
    out_left %>% show_query(),
    expr(
      setnames(
        setcolorder(
          copy(dt2)[dt1, on = .(a2 = a1), allow.cartesian = TRUE],
          !!c("a2", "i.z", "z")
        ),
        !!c("a2", "i.z", "z"),
        !!c("a1", "z.x", "z.y")
      )
    )
  )
  expect_setequal(tbl_vars(out_left), c("a1", "z.x", "z.y"))
})

test_that("setnames only used when necessary", {
  dt1 <- lazy_dt(data.frame(x = 1:2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 2:3, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x") %>% show_query(),
    expr(setcolorder(dt2[dt1, on = .(x), allow.cartesian = TRUE], !!c("x", "a", "b")))
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
  dt2 <- lazy_dt(data.frame(x = 1, y = 22, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x", suffix = c("X", "Y")) %>% show_query(),
    expr(
      setnames(
        setcolorder(
          copy(dt2)[dt1, on = .(x), allow.cartesian = TRUE],
          !!c("x", "i.y", "a", "y", "b")
        ),
        !!c("i.y", "y"),
        !!c("y.x", "y.y")
      )
    )
  )
})

test_that("automatically data.frame converts to lazy_dt", {
  dt1 <- lazy_dt(data.frame(x = 1, y = 2, a = 3), "dt1")
  df2 <- data.frame(x = 1, y = 2, a = 3)

  out <- left_join(dt1, df2, by = "x")
  expect_s3_class(out, "dtplyr_step")
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

test_that("performs cartesian joins as needed", {
  x <- lazy_dt(data.frame(x = c(2, 2, 2), y = 1:3))
  y <- lazy_dt(data.frame(x = c(2, 2, 2), z = 1:3))
  out <- collect(left_join(x, y, by = "x"))
  expect_equal(nrow(out), 9)
})
