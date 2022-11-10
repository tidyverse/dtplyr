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
    exprs(a_1 = log(a), a_2 = exp(a))
  )
})

test_that("across() captures anonymous functions", {
  dt <- lazy_dt(data.frame(a = 1))

  expect_equal(
   capture_across(dt, across(a, function(x) log(x))),
   list(a = call2(function(x) log(x), quote(a)))
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
    capture_across(dt, across(a:b, ~2)),
    exprs(a = 2, b = 2)
  )

  expect_equal(
    capture_across(dt, across(a:b, list(~log(.x)))),
    exprs(a_1 = log(a), b_1 = log(b))
  )
})

test_that("across() does not support formulas with dots", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))

  expect_snapshot({
    (expect_error(capture_across(dt, across(a:b, ~log(.x, base = .y), base = 2))))
    (expect_error(capture_across(dt, across(a:b, list(~log(.x, base = .y)), base = 2))))
  })
})

test_that("across() gives informative errors", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))
  expect_snapshot(error = TRUE, {
    capture_across(dt, across(a, 1))
    capture_across(dt, across(a, list(1)))
  })
})

test_that("across() can use named selections", {
  dt <- lazy_dt(data.frame(x = 1, y = 2))

  # no fns
  expect_equal(
    capture_across(dt, across(c(a = x, b = y))),
    list(a = quote(x), b = quote(y))
  )
  expect_equal(
    capture_across(dt, across(all_of(c(a = "x", b = "y")))),
    list(a = quote(x), b = quote(y))
  )

  # one fn
  expect_equal(
    capture_across(dt, across(c(a = x, b = y), mean)),
    list(a = quote(mean(x)), b = quote(mean(y)))
  )
  expect_equal(
    capture_across(dt, across(all_of(c(a = "x", b = "y")), mean)),
    list(a = quote(mean(x)), b = quote(mean(y)))
  )

  # multiple fns
  expect_equal(
    capture_across(dt, across(c(a = x, b = y), list(mean, nm = sum))),
    list(
      a_1 = quote(mean(x)), a_nm = quote(sum(x)),
      b_1 = quote(mean(y)), b_nm = quote(sum(y))
    )
  )
  expect_equal(
    capture_across(dt, across(all_of(c(a = "x", b = "y")), list(mean, nm = sum))),
    list(
      a_1 = quote(mean(x)), a_nm = quote(sum(x)),
      b_1 = quote(mean(y)), b_nm = quote(sum(y))
    )
  )
})

test_that("across() can handle empty selection", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% mutate(across(character(), c)) %>% show_query(),
    expr(DT)
  )
})

test_that("across() .cols is evaluated in across()'s calling environment", {
  dt <- lazy_dt(data.frame(y = 1))
  fun <- function(x) capture_across(dt, across(all_of(x)))
  expect_equal(
    fun("y"),
    list(y = expr(y))
  )
})

test_that("across() output can be used as a data frame", {
  df <- lazy_dt(tibble(x = 1:3, y = 1:3, z = c("a", "a", "b")))
  res <- df %>%
    mutate(across_df = rowSums(across(c(x, y), ~ .x + 1))) %>%
    collect()

  expect_named(res, c("x", "y", "z", "across_df"))
  expect_equal(res$across_df, c(4, 6, 8))
})

test_that("pick() works", {
  df <- lazy_dt(tibble(x = 1:3, y = 1:3, z = c("a", "a", "b")))
  res <- df %>%
    mutate(row_sum = rowSums(pick(x, y))) %>%
    collect()

  expect_named(res, c("x", "y", "z", "row_sum"))
  expect_equal(res$row_sum, c(2, 4, 6))
})

# if_all ------------------------------------------------------------------

test_that("if_all collapses multiple expresions", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))
  expect_equal(capture_if_all(dt, if_all(everything(), is.na)), expr(is.na(a) & is.na(b)))
})

test_that("if_all works without `.fns` argument", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))
  expect_equal(capture_if_all(dt, if_all(c(a:b))), expr(a & b))
})


test_that("if_all() drops groups", {
  dt <- lazy_dt(data.frame(a = 1, b = 2))

  expect_equal(
    capture_if_all(group_by(dt, a), if_all(everything())),
    sym("b")
  )
  expect_equal(
    capture_if_all(group_by(dt, b), if_all(everything())),
    sym("a")
  )
})

test_that("if_all() translates functions", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))

  expect_equal(
    capture_if_all(dt, if_all(a:b, log)),
    expr(log(a) & log(b))
  )

  expect_equal(
    capture_if_all(dt, if_all(a:b, log, base = 2)),
    expr(log(a, base = 2) & log(b, base = 2))
  )

  expect_equal(
    capture_if_all(dt, if_all(a, list(log, exp))),
    expr(log(a) & exp(a))
  )
})

test_that("if_all() captures anonymous functions", {
  dt <- lazy_dt(data.frame(a = 1))

  expect_equal(
   capture_if_all(dt, if_all(a, function(x) log(x))),
   call2(function(x) log(x), quote(a))
  )
})

test_that("if_all() translates dots", {
  fun <- function() {
    dt <- lazy_dt(data.frame(a = 1, b = 2))
    z <- TRUE
    capture_if_all(dt, if_all(a, mean, na.rm = z))
  }

  expect_equal(fun(), expr(mean(a, na.rm = TRUE)))
})

test_that("if_all() translates formulas", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))

  expect_equal(
    capture_if_all(dt, if_all(a:b, ~ log(.x))),
    expr(log(a) & log(b))
  )

  expect_equal(
    capture_if_all(dt, if_all(a:b, ~2)),
    expr(2 & 2)
  )

  expect_equal(
    capture_if_all(dt, if_all(a:b, list(~log(.x)))),
    expr(log(a) & log(b))
  )
})

test_that("if_all() gives informative errors", {
  dt <- lazy_dt(data.frame(a = 1,  b = 2))
  expect_snapshot(error = TRUE, {
    capture_if_all(dt, if_all(a, 1))
    capture_if_all(dt, if_all(a, list(1)))
  })
})

test_that("if_all() cannot rename variables", {
  dt <- lazy_dt(data.frame(x = 1, y = 2))

  # no fns
  expect_snapshot(
    (expect_error(capture_if_all(dt, if_all(c(a = x, b = y)))))
  )
})

test_that("if_all() can handle empty selection", {
  skip("tidyselect issue #221")
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% mutate(if_all(character(), c)) %>% show_query(),
    expr(DT)
  )
})

test_that("if_all() .cols is evaluated in across()'s calling environment", {
  dt <- lazy_dt(data.frame(y = 1))
  fun <- function(x) capture_if_all(dt, if_all(all_of(x)))
  expect_equal(
    fun("y"),
    expr(y)
  )
})
