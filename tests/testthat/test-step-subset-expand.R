test_that("expand completes all values", {
  tbl <- tibble(x = 1:2, y = 1:2)
  dt <- lazy_dt(tbl, "DT")
  step <- expand(dt, x, y)
  out <- collect(step)

  expect_equal(
    show_query(step),
    expr(DT[, CJ(x = x, y = y, unique = TRUE)])
  )
  expect_equal(step$vars, c("x", "y"))
  expect_equal(nrow(out), 4)
})

test_that("multiple variables in one arg doesn't expand", {
  tbl <- tibble(x = 1:2, y = 1:2)
  dt <- lazy_dt(tbl, "DT")
  step <- expand(dt, c(x, y))
  out <- collect(step)

  expect_equal(nrow(out), 2)
})

test_that("works with unnamed vectors", {
  tbl <- tibble(x = 1:2, y = 1:2)
  dt <- lazy_dt(tbl, "DT")
  step <- expand(dt, x, 1:2)
  out <- collect(step)

  expect_equal(
    show_query(step),
    expr(DT[, CJ(x = x, V2 = 1:2, unique = TRUE)])
  )
  expect_equal(step$vars, c("x", "V2"))
  expect_equal(nrow(out), 4)
})

test_that("works with named vectors", {
  tbl <- tibble(x = 1:2, y = 1:2)
  dt <- lazy_dt(tbl, "DT")
  step <- expand(dt, x, val = 1:2)
  out <- collect(step)

  expect_equal(
    show_query(step),
    expr(DT[, CJ(x = x, val = 1:2, unique = TRUE)])
  )
  expect_equal(step$vars, c("x", "val"))
  expect_equal(nrow(out), 4)
})

test_that("expand respects groups", {
  tbl <- tibble(
    a = c(1L, 1L, 2L),
    b = c(1L, 2L, 1L),
    c = c(2L, 1L, 1L)
  )
  dt <- lazy_dt(tbl, "DT")
  step <- dt %>% group_by(c) %>% expand(a, b)
  out <- collect(step)

  expect_equal(
    show_query(step),
    expr(DT[, CJ(a = a, b = b, unique = TRUE), keyby = .(c)])
  )
  expect_equal(step$vars, c("c", "a", "b"))
  expect_equal(out$a, c(1, 1, 2, 2, 1))
  expect_equal(out$b, c(1, 2, 1, 2, 1))
})

test_that("expand handles group variables as arguments", {
  dt <- lazy_dt(data.frame(x = 1, y = 2, z = 3), "DT")

  # single group var, not redefined
  res <- dt %>% group_by(x) %>% expand(x, y)
  expect_equal(
    show_query(res),
    expr(DT[, CJ(x = x, y = y, unique = TRUE), keyby = .(x)][, `:=`("x", NULL)])
  )
  expect_equal(
    res$groups,
    "x"
  )

  # multiple group vars, not redefined
  res <- dt %>% group_by(x, y) %>% expand(x, y, z)
  expect_equal(
    show_query(res),
    expr(DT[, CJ(x = x, y = y, z = z, unique = TRUE), keyby = .(x, y)
            ][, !!expr(!!c("x", "y") := NULL)])
  )
  expect_equal(
    res$groups,
    c("x", "y")
  )

  # redefined group var
  res <- dt %>% group_by(x) %>% expand(x = 5, y)
  expect_equal(
    show_query(res),
    expr(DT[, CJ(x = 5, y = y, unique = TRUE), keyby = .(x)][, `:=`("x", NULL)])
  )
  expect_equal(
    res$groups,
    c("x")
  )
  expect_equal(
    as_tibble(res),
    tibble(x = 5, y = 2)
  )
})

test_that("NULL inputs", {
  tbl <- tibble(x = 1:5)
  dt <- lazy_dt(tbl, "DT")
  step <- expand(dt, x, y = NULL)
  out <- collect(step)
  expect_equal(out, tbl)
})

test_that("expand respects .name_repair", {
  dt <- lazy_dt(tibble(x = 1:2), "DT")

  suppressMessages(
    expect_named(dt %>% expand(x, x, .name_repair = "unique") %>% collect(), c("x...1", "x...2"))
  )
})
