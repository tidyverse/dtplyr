test_that("can select variables", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% select(-z) %>% show_query(),
    expr(DT[, .(x, y)])
  )

  expect_equal(
    dt %>% select(a = x, y) %>% show_query(),
    expr(DT[, .(a = x, y)])
  )
})

test_that("can merge iff j-generating call comes after i", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% filter(x > 1) %>% select(y) %>% show_query(),
    expr(DT[x > 1, .(y)])
  )
  expect_equal(
    dt %>% select(x = y) %>% filter(x > 1) %>% show_query(),
    expr(DT[, .(x = y)][x > 1])
  )
})

test_that("renames grouping vars", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1))
  gt <- group_by(dt, x)

  expect_equal(select(gt, y = x)$groups, "y")
})

test_that("empty select returns no columns", {
  dt <- data.table(x = 1, y = 1, z = 1)
  lz <- lazy_dt(dt, "DT")
  expect_equal(
    lz %>% select() %>% collect(),
    tibble()
  )

  # unless it's grouped
  skip_if(utils::packageVersion("rlang") < "0.5.0")
  expect_snapshot(out <- lz %>% group_by(x) %>% select())
  expect_equal(
    out %>% collect(),
    group_by(tibble(x = 1), x)
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% select(a = x, y) %>% .$vars, c("a", "y"))
})

test_that("only add step if necessary", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3), "DT")
  expect_equal(dt %>% select(everything()), dt)
  expect_equal(dt %>% select(x, y), dt)
})
