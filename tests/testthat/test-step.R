
# copying -----------------------------------------------------------------

test_that("need to copy when there's a mutate", {
  dt <- lazy_dt(data.table(x = 1))

  expect_false(dt %>% dt_needs_copy())
  expect_false(dt %>% filter(x == 1) %>% dt_needs_copy())
  expect_false(dt %>% head() %>% dt_needs_copy())

  expect_true(dt %>% mutate(y = 1) %>% dt_needs_copy())
  expect_true(dt %>% mutate(y = 1) %>% filter(x == 1) %>% dt_needs_copy())
  expect_true(dt %>% mutate(y = 1) %>% head() %>% dt_needs_copy())
})

test_that("unless there's already an implicit copy", {
  dt <- lazy_dt(data.table(x = 1))

  expect_true(dt %>% filter(x == 1) %>% dt_implicit_copy())
  expect_false(dt %>% filter(x == 1) %>% mutate(y = 1) %>% dt_needs_copy())

  expect_true(dt %>% head() %>% dt_implicit_copy())
  expect_false(dt %>% head() %>% mutate(y = 1) %>% dt_needs_copy())
})
