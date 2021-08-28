test_that("across() translates NULL", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))

  expect_equal(
    capture_across(dt, across(a:b)),
    list(a = expr(a), b = expr(b))
  )
})

test_that("across() drops groups", {
  dt <- lazy_dt(data.frame(a = 1, b = 2))

  expect_equal(
    capture_across(group_by(dt, a), across(everything())),
    list(b = expr(b))
  )
  expect_equal(
    capture_across(group_by(dt, b), across(everything())),
    list(a = expr(a))
  )
})

test_that("across() translates character vectors", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))

  expect_equal(
    capture_across(dt, across(a:b, "log")),
    exprs(a = log(a), b = log(b))
  )

  expect_equal(
    capture_across(dt, across(a:b, "log", base = 2)),
    exprs(a = log(a, base = 2), b = log(b, base = 2))
  )

  expect_equal(
    capture_across(dt, across(a, c("log", "exp"))),
    exprs(a_log = log(a), a_exp = exp(a))
  )
})

test_that("across() translates functions", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))

  expect_equal(
    capture_across(dt, across(a:b, log)),
    exprs(a = log(a), b = log(b))
  )

  expect_equal(
    capture_across(dt, across(a:b, log, base = 2)),
    exprs(a = log(a, base = 2), b = log(b, base = 2))
  )

  expect_equal(
    capture_across(dt, across(a, list(log, exp))),
    exprs(a_log = log(a), a_exp = exp(a))
  )
})

test_that("dots are translated too", {
  fun <- function() {
    dt <- lazy_dt(data.frame(a = 1, b = 2))
    z <- TRUE
    capture_across(dt, across(a, mean, na.rm = z))
  }

  expect_equal(fun(), exprs(a = mean(a, na.rm = TRUE)))
})

test_that("across() translates formulas", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))

  expect_equal(
    capture_across(dt, across(a:b, ~ log(.x))),
    exprs(a = log(a), b = log(b))
  )

  # and recursively translates
  expect_equal(
    capture_across(dt, across(a, ~ .x / n())),
    exprs(a = a / .N)
  )

  expect_equal(
    capture_across(dt, across(a:b, list(~log(.x)))),
    exprs(a = log(a), b = log(b))
  )
})

test_that("across() gives informative errors", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))
  expect_snapshot(error = TRUE, {
    capture_across(dt, across(a, 1))
    capture_across(dt, across(a, list(1)))
  })
})

test_that("across() can use named selections", {
  df <- lazy_dt(data.frame(x = 1, y = 2))

  # no fns
  expect_equal(
    df %>% summarise(across(c(a = x, b = y))) %>% as.data.frame(),
    data.frame(a = 1, b = 2)
  )
  expect_equal(
    df %>% summarise(across(all_of(c(a = "x", b = "y")))) %>% as.data.frame(),
    data.frame(a = 1, b = 2)
  )

  # one fn
  expect_equal(
    df %>% summarise(across(c(a = x, b = y), mean)) %>% as.data.frame(),
    data.frame(a = 1, b = 2)
  )
  expect_equal(
    df %>% summarise(across(all_of(c(a = "x", b = "y")), mean)) %>% as.data.frame(),
    data.frame(a = 1, b = 2)
  )

  # multiple fns
  expect_equal(
    df %>% summarise(across(c(a = x, b = y), list(mean = mean, sum = sum))) %>% as.data.frame(),
    data.frame(a_mean = 1, a_sum = 1, b_mean = 2, b_sum = 2)
  )
  expect_equal(
    df %>% summarise(across(all_of(c(a = "x", b = "y")), list(mean = mean, sum = sum))) %>% as.data.frame(),
    data.frame(a_mean = 1, a_sum = 1, b_mean = 2, b_sum = 2)
  )

})

test_that("across() can handle empty selection", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% mutate(across(character(), c)) %>% show_query(),
    expr(copy(DT)[, .SD])
  )
})


# if_all ------------------------------------------------------------------

test_that("if_all translations names, strings, and formulas", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))

  expect_equal(capture_if_all(dt, if_all(a, is.na)), expr(is.na(a)))
  expect_equal(capture_if_all(dt, if_all(a, "is.na")), expr(is.na(a)))
  expect_equal(capture_if_all(dt, if_all(a, ~ is.na(.))), expr(is.na(a)))
})

test_that("if_all collapses multiple expresions", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))
  expect_equal(capture_if_all(dt, if_all(everything(), is.na)), expr(is.na(a) & is.na(b)))
})
