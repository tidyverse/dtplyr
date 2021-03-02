
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

test_that("tally works", {
  dt <- lazy_dt(data.table(x = c(1, 1, 1, 2)), "DT")
  expect_equal(
    dt %>% group_by(x) %>% tally() %>% collect(),
    tibble(x = c(1, 2), n = c(3, 1))
  )
})
