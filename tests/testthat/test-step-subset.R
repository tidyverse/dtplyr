test_that("construtor has sensible defaults", {
  first <- step_first(data.table(x = 1), "DT")
  step <- step_subset(first)

  expect_s3_class(step, "dtplyr_step_subset")
  expect_equal(step$parent, first)
  expect_equal(step$vars, "x")
  expect_equal(step$groups, character())
  expect_equal(step$i, NULL)
  expect_equal(step$j, NULL)
})

test_that("generates expected calls", {
  first <- lazy_dt(data.table(x = 1), "DT")

  ungrouped <- step_subset(first, i = quote(i), j = quote(j))
  expect_equal(dt_call(ungrouped), expr(DT[i, j]))

  with_i <- step_subset(first, i = quote(i), j = quote(j), groups = "x")
  expect_equal(dt_call(with_i), expr(DT[i, j, keyby = .(x)]))

  without_i <- step_subset(first, j = quote(j), groups = "x")
  expect_equal(dt_call(without_i), expr(DT[, j, keyby = .(x)]))
})

# dplyr methods -----------------------------------------------------------

test_that("simple calls generate expected translations", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% transmute(x) %>% show_query(),
    expr(DT[, .(x = x)])
  )
})


# count -------------------------------------------------------------------

test_that("can be used grouped or ungrouped", {
  dt <- lazy_dt(data.table(x = c(1, 1, 1, 2)), "DT")

  expect_equal(
    dt %>% count(x) %>% collect(),
    tibble(x = c(1, 2), n = c(3, 1))
  )
  expect_equal(
    dt %>% group_by(x) %>% count() %>% collect(),
    tibble(x = c(1, 2), n = c(3, 1))
  )
})

test_that("can control name", {
  dt <- lazy_dt(data.table(x = c(1, 1, 1, 2)), "DT")

  expect_equal(
    dt %>% count(x, name = "y") %>% collect(),
    tibble(x = c(1, 2), y = c(3, 1))
  )
  expect_snapshot(
    dt %>% count(name = 10) %>% collect(),
    error = TRUE
  )
})


test_that("can weight", {
  dt <- lazy_dt(data.table(x = c(1, 1, 2), y = c(1, 2, 10)), "DT")
  expect_equal(
    dt %>% count(x, wt = y) %>% collect(),
    tibble(x = c(1, 2), n = c(3, 10))
  )
})

test_that("can sort", {
  dt <- lazy_dt(data.table(x = c(1, 1, 2), y = c(1, 2, 10)), "DT")
  expect_equal(
    dt %>% count(x, wt = y, sort = TRUE) %>% collect(),
    tibble(x = c(2, 1), n = c(10, 3))
  )
})

# do ----------------------------------------------------------------------

test_that("basic operation as expected", {
  dt <- lazy_dt(data.frame(g = c(1, 1, 2), x = 1:3), "DT")

  expect_equal(
    dt %>% do(y = ncol(.)) %>% show_query(),
    expr(DT[, .(y = .(ncol(.SD)))])
  )

  expect_equal(
    dt %>% group_by(g) %>% do(y = ncol(.)) %>% show_query(),
    expr(DT[, .(y = .(ncol(.SD))), keyby = .(g)])
  )
})


# transmute ---------------------------------------------------------------

test_that("transmute generates compound expression if needed", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% transmute(x2 = x * 2, x4 = x2 * 2) %>% show_query(),
    expr(DT[, {
      x2 <- x * 2
      x4 <- x2 * 2
      .(x2 = x2, x4 = x4)
    }])
  )
})
