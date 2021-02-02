
test_that("can slice", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% slice() %>% show_query(),
    expr(DT)
  )
  expect_equal(
    dt %>% slice(1:4) %>% show_query(),
    expr(DT[1:4])
  )
  expect_equal(
    dt %>% slice(1, 2, 3) %>% show_query(),
    expr(DT[c(1, 2, 3)])
  )
})

test_that("can slice when grouped", {
  dt1 <- lazy_dt(data.table(x = c(1, 1, 2, 2), y = c(1, 2, 3, 4)), "DT")
  dt2 <- dt1 %>% group_by(x) %>% slice(1)

  expect_equal(
    dt2 %>% show_query(),
    expr(DT[DT[, .I[1], by = .(x)]$V1])
  )
  expect_equal(as_tibble(dt2), tibble(x = c(1, 2), y = c(1, 3)))
})

test_that("slicing doesn't sorts groups", {
  dt <- lazy_dt(data.table(x = 2:1))
  expect_equal(
    dt %>% group_by(x) %>% slice(1) %>% pull(x),
    2:1
  )
})

# sample ------------------------------------------------------------------

test_that("basic usage generates expected calls", {
  dt <- lazy_dt(data.table(x = 1:5, y = 1), "DT")

  expect_equal(
    dt %>% sample_n(3) %>% show_query(),
    expr(DT[sample(.N, 3)])
  )
  expect_equal(
    dt %>% sample_frac(0.5) %>% show_query(),
    expr(DT[sample(.N, .N * 0.5)])
  )

  expect_equal(
    dt %>% sample_n(3, replace = TRUE) %>% show_query(),
    expr(DT[sample(.N, 3, replace = TRUE)])
  )
  expect_equal(
    dt %>% sample_n(3, weight = y) %>% show_query(),
    expr(DT[sample(.N, 3, prob = y)])
  )
})
