test_that("constructor has sensible defaults", {
  first <- new_step_first(data.table(x = 1), "DT")
  step <- new_step_mutate(first)

  expect_s3_class(step, "dtplyr_step_mutate")
  expect_equal(step$parent, first)
  expect_equal(step$vars, "x")
  expect_equal(step$groups, character())
  expect_equal(step$new_vars, list())
})

test_that("generates single calls as expect", {
  first <- new_step_first(data.table(x = 1), "DT")

  new_vars <- list(x2 = quote(x * 2))
  ungrouped <- new_step_mutate(first, new_vars = new_vars)
  expect_equal(dt_call(ungrouped), expr(copy(DT)[, `:=`(x2 = x * 2)]))

  grouped <- new_step_mutate(first, new_vars = new_vars, groups = "x")
  expect_equal(dt_call(grouped), expr(copy(DT)[, `:=`(x2 = x * 2), by = .(x)]))
})


# dplyr verbs -------------------------------------------------------------

test_that("mutate generates multiple steps if needed", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% mutate(x2 = x * 2, x4 = x2 * 2) %>% show_query(),
    expr(copy(DT)[, `:=`(x2 = x * 2)][, `:=`(x4 = x2 * 2)])
  )
})

test_that("transmute generates multiple steps if needed", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% transmute(x2 = x * 2, x4 = x2 * 2) %>% show_query(),
    expr(copy(DT)[, `:=`(x2 = x * 2)][, .(x2, x4 = x2 * 2)])
  )
})
