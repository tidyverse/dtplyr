test_that("basic operation as expected", {
  dt <- lazy_dt(data.frame(g = c(1, 1, 2), x = 1:3), "DT")

  expect_equal(
    dt %>% do(y = ncol(.)) %>% show_query(),
    expr(DT[, .(y = .(ncol(.SD)))])
  )

  expect_equal(
    dt %>% group_by(g) %>% do(y = ncol(.)) %>% show_query(),
    expr(DT[, .(y = .(ncol(.SD))), keyby = .(g)])
  )
})

