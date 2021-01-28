test_that("group_modify creates modified data frame", {
  dt <- lazy_dt(data.table(g = c(1, 1, 2), x = 1:3))

  foo <- function(rows, g) {
    list(nc = ncol(rows), nr = nrow(rows))
  }
  out <- dt %>% group_by(g) %>% group_modify(foo) %>% collect()

  expect_equal(out$nc, c(1, 1))
  expect_equal(out$nr, c(2, 1))
})

test_that("group_map works", {
  dt <- lazy_dt(data.table(g = c(1, 1, 2), x = 1:3))
  out <- dt %>% group_by(g) %>% group_map(~ nrow(.))
  expect_equal(out, list(2, 1))

  # don't include group data
  out <- dt %>% group_by(g) %>% group_map(~ ncol(.))
  expect_equal(out, list(1, 1))
})
