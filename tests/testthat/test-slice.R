context("slice")

test_that("slicing data.table yields same output as slicing data.frame ", {
  tbls <- list(mtcars, mtcars %>% tbl_dt())
  compare_tbls(tbls, function(x) x %>% group_by(cyl) %>% slice(c(1, 3)))
})

test_that("slicing data table preserves input class", {
  mtcars_dt <- mtcars %>% data.table()

  expect_is(mtcars_dt %>% slice(1), "data.table")
  expect_is(mtcars_dt %>% tbl_dt() %>% slice(1), "tbl_dt")
  expect_is(mtcars_dt %>% group_by(cyl) %>% slice(1), "grouped_dt")
})

