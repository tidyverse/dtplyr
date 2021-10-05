test_that("simple calls generate expected translations", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% summarise(x = mean(x)) %>% show_query(),
    expr(DT[, .(x = mean(x))])
  )

  expect_equal(
    dt %>% transmute(x) %>% show_query(),
    expr(DT[, .(x = x)])
  )
})

test_that("can use with across", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% summarise(across(x:y, mean)) %>% show_query(),
    expr(DT[, .(x = mean(x), y = mean(y))])
  )
})

test_that("can merge iff j-generating call comes after i", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% filter(x > 1) %>% summarise(y = mean(x)) %>% show_query(),
    expr(DT[x > 1, .(y = mean(x))])
  )
  expect_equal(
    dt %>% summarise(y = mean(x)) %>% filter(y > 1) %>% show_query(),
    expr(DT[, .(y = mean(x))][y > 1])
  )
})

test_that("summarise peels off layer of grouping", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1))
  gt <- group_by(dt, x, y)

  expect_equal(summarise(gt)$groups, "x")
  expect_equal(summarise(summarise(gt))$groups, character())
})

test_that("summarises sorts groups", {
  dt <- lazy_dt(data.table(x = 2:1))
  expect_equal(
    dt %>% group_by(x) %>% summarise(n = n()) %>% pull(x),
    1:2
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% summarise(z = mean(x)) %>% .$vars, "z")
  expect_equal(dt %>% group_by(y) %>% summarise(z = mean(x)) %>% .$vars, c("y", "z"))
})

test_that("empty summarise returns unique groups", {
  dt <- lazy_dt(data.table(x = c(1, 1, 2), y = 1, z = 1), "DT")

  expect_equal(
    dt %>% group_by(x) %>% summarise() %>% show_query(),
    expr(unique(DT[, .(x)]))
  )

  # If no groups, return null data.table
  expect_equal(
    dt %>% summarise() %>% show_query(),
    expr(DT[, 0L])
  )
})

test_that("if for unsupported resummarise", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_error(dt %>% summarise(x = mean(x), x2 = sd(x)), "mutate")
})

test_that("summarise(.groups=)", {
  # the `dplyr::` prefix is needed for `check()`
  # should produce a message when called directly by user
  expect_message(eval_bare(
    expr(lazy_dt(data.frame(x = 1, y = 2), "DT") %>% group_by(x, y) %>% dplyr::summarise() %>% show_query()),
    env(global_env())
  ))
  skip_if(utils::packageVersion("rlang") < "0.5.0")
  expect_snapshot(eval_bare(
    expr(lazy_dt(data.frame(x = 1, y = 2), "DT") %>% group_by(x, y) %>% dplyr::summarise() %>% show_query()),
    env(global_env())
  ))

  # should be silent when called in another package
  expect_silent(eval_bare(
    expr(lazy_dt(data.frame(x = 1, y = 2), "DT") %>% group_by(x, y) %>% dplyr::summarise() %>% show_query()),
    asNamespace("testthat")
  ))

  df <- lazy_dt(data.table(x = 1, y = 2), "DT") %>% group_by(x, y)
  expect_equal(df %>% summarise() %>% group_vars(), "x")
  expect_equal(df %>% summarise(.groups = "drop_last") %>% group_vars(), "x")
  expect_equal(df %>% summarise(.groups = "drop") %>% group_vars(), character())
  expect_equal(df %>% summarise(.groups = "keep") %>% group_vars(), c("x", "y"))

  expect_snapshot_error(df %>% summarise(.groups = "rowwise"))
})

test_that("can change group vars", {
  dt <- lazy_dt(data.frame(a = 1), "DT") %>% group_by(a)

  res <- dt %>% summarise(a = 2)
  expect_equal(
    show_query(res),
    expr(DT[, .(a = 2), keyby = .(a)][, `:=`("a", NULL)])
  )
  expect_equal(
    as_tibble(res), tibble(a = 2)
  )

  # but not with across
  expect_error(
    dt %>% summarise(across(a, ~ 2)), "Column `a` doesn't exist"
  )
})
