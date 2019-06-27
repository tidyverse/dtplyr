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
