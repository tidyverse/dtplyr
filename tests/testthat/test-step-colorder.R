test_that("can reorder columns", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1), "DT")

  expect_equal(
    dt %>% step_colorder(c("y", "x")) %>% show_query(),
    expr(setcolorder(copy(DT), !!c("y", "x")))
  )

  expect_named(
    dt %>% step_colorder(c("y", "x")) %>% collect(),
    c("y", "x")
  )
})
