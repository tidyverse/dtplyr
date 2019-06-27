test_that("grouping and ungrouping adjust groups field", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))

  expect_equal(dt %>% .$groups, character())
  expect_equal(dt %>% group_by(x) %>% .$groups, "x")
  expect_equal(dt %>% group_by(x) %>% ungroup() %>% .$groups, character())
})
