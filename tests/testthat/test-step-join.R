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
  dt1 <- lazy_dt(tibble(x = 1, y = 2, a = 3), "dt1")
  dt2 <- lazy_dt(tibble(x = 1, y = 2, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x") %>% show_query(),
    expr(
      setnames(
        setcolorder(
          dt2[dt1, on = .(x), allow.cartesian = TRUE],
          !!c(1L, 4L, 5L, 2L, 3L)
        ),
        !!c("i.y", "y"),
        !!c("y.x", "y.y")
      )
    )
  )

  expect_equal(
    dt1 %>% right_join(dt2, by = "x") %>% show_query(),
    expr(
      setnames(
        dt1[dt2, on = .(x), allow.cartesian = TRUE],
        !!c("y", "i.y"),
        !!c("y.x", "y.y")
      )
    )
  )

  expect_equal(
    dt1 %>% inner_join(dt2, by = "x") %>% show_query(),
    expr(
      setnames(
        dt1[dt2, on = .(x), nomatch = NULL, allow.cartesian = TRUE],
        !!c("y", "i.y"),
        !!c("y.x", "y.y")
      )
    )
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

test_that("full_join produces correct names", {
  # data.table: use merge which simply appends the corresponding suffix
  #   producing duplicates
  # dplyr: appends suffix until name is unique
  df1 <- tibble(a = "a", b = "b.x", b.x = "b.x.x.x")
  df2 <- tibble(a = "a", b = "b.y", b.x.x = "b.x.x")

  dt1 <- lazy_dt(df1, "dt1")
  dt2 <- lazy_dt(df2, "dt2")

  joined_dt <- full_join(dt1, dt2, by = "a")
  expected <- full_join(df1, df2, by = "a") %>% colnames()

  expect_equal(
    joined_dt %>% .$vars,
    expected
  )

  # suppress warning created by `data.table::merge()`
  expect_equal(
    suppressWarnings(joined_dt %>% collect() %>% colnames()),
    expected
  )
})

test_that("join can handle `by` where order doesn't match input", {
  dt1 <- lazy_dt(tibble(a = "a", b = "b", c = "c"), name = "dt1")
  dt2 <- lazy_dt(tibble(a = "a", b = "b", c = "c", d = "d"), name = "dt2")

  dt3 <- left_join(dt1, dt2, by = c("c", "b", "a"))
  expect_equal(dt3$vars, letters[1:4])
  expect_equal(collect(dt3), collect(dt2))

  dt4 <- full_join(dt1, dt2, by = c("c", "b", "a"))
  expect_equal(dt4$vars, letters[1:4])
  expect_equal(collect(dt4), collect(dt2))

  dt5 <- left_join(dt1, dt2, by = c("c", "b"))
  expect_equal(
    collect(dt5),
    tibble(a.x = "a", b = "b", c = "c", a.y = "a", d = "d")
  )
})

test_that("left_join produces correct names", {
  # data.table: uses y[x] which prefixes `x` vars with "i." and if name is not
  #   unique it appends ".<number>" with the smallest number without a collision
  # dplyr: appends suffix until name is unique
  df1 <- tibble(a = "a", b = "b.x", i.b = "i.b")
  df2 <- tibble(a = "a", b = "b.y")

  dt1 <- lazy_dt(df1, "dt1")
  dt2 <- lazy_dt(df2, "dt2")

  joined_dt <- left_join(dt1, dt2, by = "a")
  expected <- left_join(df1, df2, by = "a") %>% colnames()

  expect_equal(
    joined_dt %>% .$vars,
    expected
  )

  expect_equal(
    joined_dt %>% collect() %>% colnames(),
    expected
  )
})

test_that("named by converted to by.x and by.y", {
  dt1 <- lazy_dt(data.frame(a1 = 1:3, z = 1), "dt1")
  dt2 <- lazy_dt(data.frame(a2 = 1:3, z = 2), "dt2")

  out_inner <- inner_join(dt1, dt2, by = c('a1' = 'a2'))
  expect_equal(
    out_inner %>% show_query(),
    expr(
      setnames(
        dt1[dt2, on = .(a1 = a2), nomatch = NULL, allow.cartesian = TRUE],
        !!c("z", "i.z"),
        !!c("z.x", "z.y")
      )
    )
  )
  expect_setequal(tbl_vars(out_inner), c("a1", "z.x", "z.y"))

  out_left <- left_join(dt1, dt2, by = c('a1' = 'a2'))
  expect_equal(
    out_left %>% show_query(),
    expr(
      setnames(
        setcolorder(
          dt2[dt1, on = .(a2 = a1), allow.cartesian = TRUE],
          !!c(1L, 3L, 2L)
        ),
        !!c("a2", "i.z", "z"),
        !!c("a1", "z.x", "z.y")
      )
    )
  )
  expect_setequal(tbl_vars(out_left), c("a1", "z.x", "z.y"))
})

test_that("named by can handle edge cases", {
  test_equal <- function(f_join) {
    joined_dt <- f_join(dt1, dt2, by = c("x", z = "y"))
    expected <- f_join(df1, df2, by = c("x", z = "y"))

    expect_equal(
      joined_dt %>% collect(),
      expected
    )

    expect_equal(
      joined_dt$vars,
      colnames(expected)
    )
  }

  df1 <- tibble(x = 1, y = 1, z = 2)
  df2 <- tibble(x = 1, y = 2)

  dt1 <- lazy_dt(df1, "dt1")
  dt2 <- lazy_dt(df2, "dt2")

  test_equal(left_join)
  test_equal(right_join)
  test_equal(full_join)

  test_equal(semi_join)
  test_equal(anti_join)
})

test_that("setnames only used when necessary", {
  dt1 <- lazy_dt(data.frame(x = 1:2, a = 3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 2:3, b = 4), "dt2")

  expect_equal(
    dt1 %>% left_join(dt2, by = "x") %>% show_query(),
    expr(setcolorder(dt2[dt1, on = .(x), allow.cartesian = TRUE], !!c(1L, 3L, 2L)))
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
          dt2[dt1, on = .(x), allow.cartesian = TRUE],
          !!c(1L, 4L, 5L, 2L, 3L)
        ),
        !!c("i.y", "y"),
        !!c("yX", "yY")
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

test_that("performs cross join", {
  df1 <- data.frame(x = 1:2, y = "a", stringsAsFactors = FALSE)
  df2 <- data.frame(x = 3:4)

  dt1 <- lazy_dt(df1, "dt1")
  dt2 <- lazy_dt(df2, "dt2")
  expected <- left_join(df1, df2, by = character()) %>% as_tibble()

  expect_snapshot(left_join(dt1, dt2, by = character()))
  expect_equal(left_join(dt1, dt2, by = character()) %>% collect(), expected)

  expect_snapshot(right_join(dt1, dt2, by = character()))
  expect_equal(right_join(dt1, dt2, by = character()) %>% collect(), expected)

  expect_snapshot(full_join(dt1, dt2, by = character()))
  expect_equal(full_join(dt1, dt2, by = character()) %>% collect(), expected)

  expect_snapshot(inner_join(dt1, dt2, by = character()))
  expect_equal(inner_join(dt1, dt2, by = character()) %>% collect(), expected)
})
