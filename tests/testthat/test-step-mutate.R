test_that("constructor has sensible defaults", {
  first <- step_first(data.table(x = 1), "DT")
  step <- step_mutate(first)

  expect_s3_class(step, "dtplyr_step_mutate")
  expect_equal(step$parent, first)
  expect_equal(step$vars, "x")
  expect_equal(step$groups, character())
  expect_equal(step$new_vars, list())
})

# copies ------------------------------------------------------------------

test_that("need to copy when there's a mutate", {
  dt <- lazy_dt(data.table(x = 1))

  expect_false(dt %>% .$needs_copy)
  expect_false(dt %>% filter(x == 1) %>% .$needs_copy)
  expect_false(dt %>% head() %>% .$needs_copy)

  expect_true(dt %>% mutate(y = 1) %>% .$needs_copy)
  expect_true(dt %>% mutate(y = 1) %>% filter(x == 1) %>% .$needs_copy)
  expect_true(dt %>% mutate(y = 1) %>% head() %>% .$needs_copy)
})

test_that("unless there's already an implicit copy", {
  dt <- lazy_dt(data.table(x = 1))

  expect_true(dt %>% filter(x == 1) %>% .$implicit_copy)
  expect_false(dt %>% filter(x == 1) %>% mutate(y = 1) %>% .$needs_copy)

  expect_true(dt %>% head() %>% .$implicit_copy)
  expect_false(dt %>% head() %>% mutate(y = 1) %>% .$needs_copy)
})

# dplyr verbs -------------------------------------------------------------

test_that("generates single calls as expect", {
  dt <- lazy_dt(data.table(x = 1), "DT")

  expect_equal(
    dt %>% mutate(x2 = x * 2) %>% show_query(),
    expr(copy(DT)[, `:=`(x2 = x * 2)])
  )

  expect_equal(
    dt %>% group_by(x) %>% mutate(x2 = x * 2) %>% show_query(),
    expr(copy(DT)[, `:=`(x2 = x * 2), by = .(x)])
  )

  expect_equal(
    dt %>% transmute(x2 = x * 2) %>% show_query(),
    expr(DT[, .(x2 = x * 2)])
  )
})

test_that("mutate generates compound expression if needed", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% mutate(x2 = x * 2, x4 = x2 * 2) %>% show_query(),
    expr(copy(DT)[, c("x2", "x4") := {
      x2 <- x * 2
      x4 <- x2 * 2
      .(x2, x4)
    }])
  )
})

test_that("allows multiple assignment to the same variable", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% mutate(x = x * 2, x = x * 2) %>% show_query(),
    expr(copy(DT)[, c("x") := {
      x <- x * 2
      x <- x * 2
      .(x)
    }])
  )
})

test_that("can use across", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% mutate(across(everything(), ~ . + 1)) %>% show_query(),
    expr(copy(DT)[, `:=`(x = x + 1, y = y + 1)])
  )

  expect_equal(
    dt %>% mutate(across(.fns = ~ . + 1)) %>% show_query(),
    expr(copy(DT)[, `:=`(x = x + 1, y = y + 1)])
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% mutate(z = 1) %>% .$vars, c("x", "y", "z"))
  expect_equal(dt %>% mutate(x = NULL, z = 1) %>% .$vars, c("y", "z"))
})

test_that("emtpy mutate returns input", {
  dt <- lazy_dt(data.frame(x = 1))
  expect_equal(mutate(dt), dt)
  expect_equal(mutate(dt, !!!list()), dt)
})

# .before and .after -----------------------------------------------------------

test_that("can use .before and .after to control column position", {
  dt <- lazy_dt(data.frame(x = 1, y = 2))
  expect_named(
    mutate(dt, z = 1) %>% as_tibble(),
    c("x", "y", "z")
  )
  expect_named(
    mutate(dt, z = 1, .before = x) %>% as_tibble(),
    c("z", "x", "y")
  )
  expect_named(
    mutate(dt, z = 1, .after = x) %>% as_tibble(),
    c("x", "z", "y")
  )

  # but doesn't affect order of existing columns
  expect_named(
    mutate(dt, x = 1, .after = y) %>% as_tibble(),
    c("x", "y")
  )
})
