test_that("grouping and ungrouping adjust groups field", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))

  expect_equal(dt %>% .$groups, character())
  expect_equal(dt %>% group_by(x) %>% .$groups, "x")
  expect_equal(dt %>% group_by(x) %>% ungroup() %>% .$groups, character())
})

test_that("grouping can compute new variables if needed", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3), "DT")

  expect_equal(
    dt %>% group_by(xy = x + y) %>% show_query(),
    expr(copy(DT)[, `:=`(xy = x + y)])
  )

  expect_equal(
    dt %>% group_by(xy = x + y) %>% summarise(x = mean(x)) %>% show_query(),
    expr(copy(DT)[, `:=`(xy = x + y)][, .(x = mean(x)), keyby = .(xy)])
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% group_by(x) %>% .$vars, c("x", "y"))
})
