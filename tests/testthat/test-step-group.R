test_that("grouping and ungrouping adjust groups field", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))

  expect_equal(dt %>% .$groups, character())
  expect_equal(dt %>% group_by(x) %>% .$groups, "x")
  expect_equal(dt %>% group_by(a = x) %>% .$groups, "a")
  expect_equal(dt %>% group_by(x) %>% group_by(y) %>% .$groups, "y")
  expect_equal(dt %>% group_by(x) %>% ungroup() %>% .$groups, character())
})

test_that("ungroup can remove variables from grouping", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3)) %>% group_by(x, y)

  expect_equal(dt %>% ungroup(y) %>% group_vars(), "x")
})

test_that("can use across", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% group_by(across(everything())) %>% .$groups, c("x", "y"))
})

test_that("can add groups if requested", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3), "DT")
  expect_equal(
    dt %>% group_by(x) %>% group_by(y, .add = TRUE) %>% .$groups,
    c("x", "y")
  )

  expect_snapshot({
    . <- dt %>% group_by(x) %>% group_by(y, add = TRUE)
  })
})

test_that("grouping can compute new variables if needed", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3), "DT")

  expect_equal(
    dt %>% group_by(xy = x + y) %>% show_query(),
    expr(copy(DT)[, `:=`(xy = x + y)])
  )

  # also works when RHS is only a symbol
  expect_equal(
    dt %>% group_by(z = x) %>% show_query(),
    expr(copy(DT)[, `:=`(z = x)])
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

test_that("`key` switches between keyby= and by=", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3), "DT")
  dt1 <- lazy_dt(mtcars, "DT1")

  expect_equal(
    dt %>% group_by(xy = x + y, arrange = FALSE) %>% summarize(x = mean(x)) %>% show_query(),
    expr(copy(DT)[, `:=`(xy = x + y)][, .(x = mean(x)), by = .(xy)])
  )

  expect_equal(
    dt1 %>% group_by(cyl, arrange = FALSE) %>% summarize(mean_mpg = mean(mpg)) %>% show_query(),
    expr(DT1[, .(mean_mpg = mean(mpg)), by = .(cyl)])
  )

  expect_equal(
    dt1 %>% group_by(cyl) %>% summarize(mean_mpg = mean(mpg)) %>% show_query(),
    expr(DT1[, .(mean_mpg = mean(mpg)), keyby = .(cyl)])
  )
})

test_that("emtpy and NULL group_by ungroups", {
  dt <- lazy_dt(data.frame(x = 1)) %>% group_by(x)
  expect_equal(group_by(dt) %>% group_vars(), character())
  expect_equal(group_by(dt, NULL) %>% group_vars(), character())
  expect_equal(group_by(dt, !!!list()) %>% group_vars(), character())
})

test_that("only adds step if necessary", {
  dt <- lazy_dt(data.table(x = 1, y = 1), "DT")
  expect_equal(dt %>% group_by(), dt)

  expect_equal(dt %>% ungroup(), dt)
  expect_equal(dt %>% ungroup(x), dt)

  dt_grouped <- dt %>% group_by(x)
  dt_grouped2 <- dt_grouped %>% group_by(x)
  expect_equal(dt_grouped, dt_grouped2)
  expect_equal(dt_grouped %>% ungroup(y), dt_grouped)

  out <- dt_grouped %>% mutate(y = y - mean(y)) %>% group_by()
  expect_s3_class(out, "dtplyr_step_group")
  expect_equal(group_vars(out), character())
})
