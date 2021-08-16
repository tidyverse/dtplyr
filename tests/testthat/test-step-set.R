test_that("basic ops generate expected translation", {
  dt1 <- lazy_dt(data.frame(x = 1:3), "dt1")
  dt2 <- lazy_dt(data.frame(x = 2L), "dt2")

  expect_equal(
    dt1 %>% intersect(dt2) %>% show_query(),
    expr(fintersect(dt1, dt2))
  )
  expect_equal(
    dt1 %>% union(dt2) %>% show_query(),
    expr(funion(dt1, dt2))
  )
  expect_equal(
    dt1 %>% union_all(dt2) %>% show_query(),
    expr(funion(dt1, dt2, all = TRUE))
  )
  expect_equal(
    dt1 %>% setdiff(dt2) %>% show_query(),
    expr(fsetdiff(dt1, dt2))
  )
})

test_that("basic ops work with data.table inputs", {
  dt1 <- data.table(x = 1:3)
  dt2 <- data.table(x = 2L)

  expect_equal(
    dt1 %>% intersect(dt2) %>% collect(),
    tibble(x = 2)
  )
  expect_equal(
    dt1 %>% union(dt2) %>% collect(),
    tibble(x = 1:3)
  )
  expect_equal(
    dt1 %>% union_all(dt2) %>% collect(),
    tibble(x = c(1:3, 2))
  )
  expect_equal(
    dt1 %>% setdiff(dt2) %>% collect(),
    tibble(x = c(1, 3))
  )
})

test_that("joins captures locals from both parents", {
  dt1 <- lazy_dt(data.frame(x = 1)) %>% mutate(y = 1) %>% compute("D1")
  dt2 <- lazy_dt(data.frame(x = 1)) %>% mutate(z = 1) %>% compute("D2")

  expect_named(intersect(dt1, dt2)$locals, c("D1", "D2"))
})

test_that("vars set correctly", {
  # data.table functions require the inputs to have same columns
  dt1 <- lazy_dt(data.frame(x = 1, y = 2), "dt1")
  dt2 <- lazy_dt(data.frame(x = 2, y = 2), "dt2")

  expect_equal(dt1 %>% union(dt2) %>% .$vars, c("x", "y"))
})
