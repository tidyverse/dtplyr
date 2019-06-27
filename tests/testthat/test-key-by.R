test_that("sets groups", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")
  expect_equal(dt %>% key_by(x) %>% .$groups, "x")
})

test_that("orders output", {
  dt <- lazy_dt(data.table(g = c(3L, 1L, 2L)), "DT")

  out <- dt %>% key_by(g) %>% tally() %>% as_tibble()
  expect_equal(out, tibble(g = 1:3, n = 1L))
})
