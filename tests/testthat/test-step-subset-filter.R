test_that("can filter by value", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% filter() %>% show_query(),
    expr(DT)
  )
  expect_equal(
    dt %>% filter(x) %>% show_query(),
    expr(DT[(x)])
  )

  expect_equal(
    dt %>% filter(x > 1) %>% show_query(),
    expr(DT[x > 1])
  )

  expect_equal(
    dt %>% filter(x > 1, y > 2) %>% show_query(),
    expr(DT[x > 1 & y > 2])
  )
})

test_that("can filter with logical columns", {
  dt <- lazy_dt(data.table(x = c(TRUE, FALSE)), "DT")

  expect_equal(
    dt %>% filter(x) %>% show_query(),
    expr(DT[(x)])
  )

  expect_equal(
    dt %>% filter(!x) %>% show_query(),
    expr(DT[(!x)])
  )
})


test_that("inlines external variables", {
  dt <- lazy_dt(data.table(x = 1), "DT")
  l <- c(1, 10)

  expect_equal(
    dt %>% filter(x %in% l) %>% show_query(),
    quote(DT[x %in% !!l])
  )

  # Except in the global environment
  # But I can't figure out how to test this - it's not too important
  # as it only affects the quality of the translation not the correctness
})

test_that("can use with across", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% filter(across(x:y, ~ . > 0)) %>% show_query(),
    expr(DT[x > 0 & y > 0])
  )

  expect_equal(
    dt %>% filter(if_all(x:y, ~ . > 0)) %>% show_query(),
    expr(DT[x > 0 & y > 0])
  )
  expect_equal(
    dt %>% filter(if_any(x:y, ~ . > 0)) %>% show_query(),
    expr(DT[x > 0 | y > 0])
  )

  # .cols defaults to everything()
  expect_equal(
    dt %>% filter(if_all(.fns = ~ . > 0)) %>% show_query(),
    expr(DT[x > 0 & y > 0 & z > 0])
  )
  expect_equal(
    dt %>% filter(if_any(.fns = ~ . > 0)) %>% show_query(),
    expr(DT[x > 0 | y > 0 | z > 0])
  )
})

test_that("can filter when grouped", {
  dt1 <- lazy_dt(data.table(x = c(1, 1, 2, 2), y = c(1, 2, 3, 4)), "DT")
  dt2 <- dt1 %>% group_by(x) %>% filter(sum(y) == 3)

  expect_equal(
    dt2 %>% show_query(),
    expr(DT[DT[, .I[sum(y) == 3], by = .(x)]$V1])
  )

  expect_equal(as_tibble(dt2), tibble(x = c(1, 1), y = c(1, 2)))
})

test_that("grouped filter doesn't reorder", {
  dt1 <- lazy_dt(data.frame(x = c(2, 2, 1, 1), y = 1:4), "DT")
  dt2 <- dt1 %>% group_by(x) %>% filter(TRUE)

  expect_equal(
    dt2 %>% show_query(),
    expr(DT[DT[, .I[TRUE], by = .(x)]$V1])
  )
  expect_equal(dt2 %>% as_tibble(), as_tibble(dt1))
})

test_that("only adds step if dots are not empty", {
  dt <- lazy_dt(data.table(x = 1), "DT")

  expect_equal(dt %>% filter(), dt)
  expect_equal(dt %>% filter(!!!list()), dt)
})

test_that("errors for named input", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_snapshot(error = TRUE, filter(dt, x = 1))
  expect_snapshot(error = TRUE, filter(dt, y > 1, x = 1))
})

test_that("allows named constants that resolve to logical vectors", {
  dt <- lazy_dt(mtcars, "DT")
  filters <- mtcars %>%
    transmute(
      cyl %in% 6:8,
      hp / drat > 50
    )

  expect_equal(
    filter(dt, !!!filters),
    filter(dt, !!!unname(filters))
  )
})
