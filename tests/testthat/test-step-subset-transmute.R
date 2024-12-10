test_that("works", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% transmute(x) %>% collect(),
    dt %>% mutate(x, .keep = "none") %>% collect()
  )
})

test_that("empty dots preserves groups", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT") %>%
    group_by(y)

  res <- dt %>% transmute() %>% collect()

  expect_equal(names(res), "y")
})

