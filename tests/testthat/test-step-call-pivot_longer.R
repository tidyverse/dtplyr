test_that("can pivot all cols to long", {
  tbl <- tibble(x = 1:2, y = 3:4)
  dt <- lazy_dt(tbl, "DT")
  step <- pivot_longer(dt, x:y)
  out <- collect(step)

  expect_equal(
    show_query(step),
    expr(melt(DT, measure.vars = !!c("x", "y"), variable.name = "name",
              variable.factor = FALSE))
  )
  expect_equal(step$vars, c("name", "value"))
  expect_equal(out$name, c("x", "x", "y", "y"))
  expect_equal(out$value, c(1, 2, 3, 4))
})

test_that("preserves original keys", {
  tbl <- tibble(x = 1:2, y = 2L, z = 1:2)
  dt <- lazy_dt(tbl, "DT")
  step <- pivot_longer(dt, y:z)
  out <- collect(step)

  expect_equal(
    show_query(step),
    expr(melt(DT, measure.vars = !!c("y", "z"), variable.name = "name",
              variable.factor = FALSE))
  )
  expect_equal(step$vars, c("x", "name", "value"))
  expect_equal(out$x, rep(tbl$x, 2))
})

test_that("can drop missing values", {
  tbl <- tibble(x = c(1, NA), y = c(NA, 2))
  dt <- lazy_dt(tbl, "DT")
  step <- pivot_longer(dt, x:y, values_drop_na = TRUE)
  out <- collect(step)

  expect_equal(
    show_query(step),
    expr(melt(DT, measure.vars = !!c("x", "y"), variable.name = "name",
              na.rm = TRUE, variable.factor = FALSE))
  )
  expect_equal(out$name, c("x", "y"))
  expect_equal(out$value, c(1, 2))
})

test_that("can pivot to multiple measure cols", {
  dt <- lazy_dt(head(anscombe, 2), "DT")
  step <- pivot_longer(
    dt,
    everything(),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)"
  )
  out <- collect(step)

  expect_snapshot(show_query(step))
  expect_equal(step$vars, c("set", "x", "y"))
})

test_that(".value can be at any position in `names_to`", {
  samp1 <- tibble(
    i = 1:4,
    y_t1 = rnorm(4),
    y_t2 = rnorm(4),
    z_t1 = rep(3, 4),
    z_t2 = rep(-2, 4),
  )
  dt1 <- lazy_dt(samp1, "DT1")

  value_first <- dt1 %>%
    pivot_longer(-i, names_to = c(".value", "time"), names_sep = "_") %>%
    collect()

  samp2 <- dplyr::rename(samp1, t1_y = y_t1,
                         t2_y = y_t2,
                         t1_z = z_t1,
                         t2_z = z_t2)
  dt2 <- lazy_dt(samp2, "DT2")

  value_second <- dt2 %>%
    pivot_longer(-i, names_to = c("time", ".value"), names_sep = "_") %>%
    collect()

  expect_identical(value_first, value_second)
})

test_that("errors on unbalanced datasets", {
  tbl <- tibble(x_1 = 1, x_2 = 1, y_3 = 1, y_4 = 1)
  dt <- lazy_dt(tbl, "DT")

  expect_snapshot(error = TRUE,
                  pivot_longer(dt, everything(), names_to = c(".value", "id"), names_sep = "_")
  )
})

test_that("can use names_prefix", {
  tbl <- tibble(x_x = 1:2, x_y = 3:4)
  dt <- lazy_dt(tbl, "DT")
  out <- dt %>%
    pivot_longer(everything(), names_prefix = "x_") %>%
    arrange(name, value) %>%
    collect()
  expect_equal(out$name, c("x","x","y","y"))
  expect_equal(out$value, c(1,2,3,4))
})

test_that("can use names_pattern w/out .value in names_to", {
  dt <- data.table(a1_1 = 1, b2_2 = 2)

  out <- dt %>%
    pivot_longer(
      cols = everything(),
      names_to = c("a", "b"),
      names_pattern = "([[:alnum:]]+)_([[:alnum:]]+)"
    ) %>%
    collect()

  expect_named(out, c("a", "b", "value"))
  expect_equal(out$a, c("a1", "b2"))
  expect_equal(out$b, c("1", "2"))
  expect_equal(out$value, c(1, 2))
})

test_that("can use names_sep w/out .value in names_to", {
  dt <- data.table(a1_1 = 1, b2_2 = 2)

  out <- dt %>%
    pivot_longer(
      cols = everything(),
      names_to = c("a", "b"),
      names_sep = "_"
    ) %>%
    collect()

  expect_named(out, c("a", "b", "value"))
  expect_equal(out$a, c("a1", "b2"))
  expect_equal(out$b, c("1", "2"))
  expect_equal(out$value, c(1, 2))
})

test_that("informative errors on unsupported features", {
  dt <- data.table(a1_1 = 1, b2_2 = 2)

  expect_snapshot(error = TRUE, {
    dt %>% pivot_longer(names_ptypes = list())
    dt %>% pivot_longer(names_transform = list())
    dt %>% pivot_longer(values_ptypes = list())
    dt %>% pivot_longer(values_transform = list())
  })

})

