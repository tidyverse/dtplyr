test_that("arrange orders variables", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% arrange(x) %>% show_query(),
    expr(DT[order(x)])
  )
})

test_that("arrange doesn't use, but still preserves, grouping", {
  dt <- group_by(lazy_dt(data.table(x = 1, y = 2), "DT"), x)

  step <- arrange(dt, y)
  expect_equal(step$groups, "x")
  expect_equal(dt_call(step), expr(DT[order(y)]))

  step2 <- arrange(dt, y, .by_group = TRUE)
  expect_equal(dt_call(step2), expr(DT[order(x, y)]))
})

test_that("empty arrange returns input unchanged", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")
  expect_true(identical(arrange(dt), dt))
})

test_that("can use with across", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% arrange(across(x:y)) %>% show_query(),
    expr(DT[order(x, y)])
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% arrange(x) %>% .$vars, c("x", "y"))
})

test_that("desc works with internal quosure", {
  dt <- lazy_dt(data.table(x = c(4,3,9,7), y = 1:4))

  desc_df <- dt %>% arrange(desc(!!quo(x))) %>% collect()

  expect_equal(desc_df$x, c(9,7,4,3))
})

test_that("only add step if necessary", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))

  expect_equal(dt %>% arrange(), dt)
  expect_equal(dt %>% arrange(!!!list()), dt)
})
