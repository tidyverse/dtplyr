
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

test_that("name can match existing group var", {
  dt <- data.table(a = 2)

  expect_equal(
    dt %>% group_by(a) %>% tally(name = 'a') %>% collect(),
    tibble(a = 1)
  )
  expect_equal(
    dt %>% count(a, name = 'a') %>% collect(),
    tibble(a = 1)
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

test_that("informs if n column already present, unless overridden", {
  dt <- lazy_dt(data.frame(n = c(1, 1, 2, 2, 2)))
  expect_message(out <- count(dt, n), "already present")
  expect_named(as_tibble(out), c("n", "nn"))

  # not a good idea, but supported
  expect_message(out <- count(dt, n, name = "n"), NA)
  expect_named(as_tibble(out), "n")

  expect_message(out <- count(dt, n, name = "nn"), NA)
  expect_named(as_tibble(out), c("n", "nn"))

  dt <- lazy_dt(data.frame(n = c(1, 1, 2, 2, 2), nn = 1:5))
  expect_message(out <- count(dt, n), "already present")
  expect_named(as_tibble(out), c("n", "nn"))

  expect_message(out <- count(dt, n, nn), "already present")
  expect_named(as_tibble(out), c("n", "nn", "nnn"))
})

test_that("name must be string", {
  dt <- lazy_dt(data.frame(x = c(1, 2)))
  expect_error(count(dt, x, name = 1), "string")
  expect_error(count(dt, x, name = letters), "string")
})
