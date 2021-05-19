test_that("simple calls generate expected translations", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% transmute(x) %>% show_query(),
    expr(DT[, .(x = x)])
  )
})

test_that("transmute generates compound expression if needed", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% transmute(x2 = x * 2, x4 = x2 * 2) %>% show_query(),
    expr(DT[, {
      x2 <- x * 2
      x4 <- x2 * 2
      .(x2 = x2, x4 = x4)
    }])
  )
})

test_that("groups are respected", {
  dt <- lazy_dt(data.table(x = 1), "DT") %>% group_by(x) %>% transmute(y = 2)

  expect_equal(dt$vars, c("x", "y"))
  expect_equal(
    dt %>% show_query(),
    expr(DT[, .(y = 2), keyby = .(x)])
  )
})
