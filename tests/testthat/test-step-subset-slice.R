
test_that("can slice", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% slice() %>% show_query(),
    expr(DT)
  )
  expect_equal(
    dt %>% slice(c(1, 2)) %>% show_query(),
    expr(DT[c(1, 2)[between(c(1, 2), -.N, .N)]])
  )
  expect_equal(
    dt %>% slice(1, 2, 3) %>% show_query(),
    expr(DT[c(1, 2, 3)[between(c(1, 2, 3), -.N, .N)]])
  )
})

test_that("can slice when grouped", {
  dt1 <- lazy_dt(data.table(x = c(1, 1, 2, 2), y = c(1, 2, 3, 4)), "DT")
  dt2 <- dt1 %>% group_by(x) %>% slice(1)

  expect_equal(
    dt2 %>% show_query(),
    expr(DT[DT[, .I[1[between(1, -.N, .N)]], by = .(x)]$V1])
  )
  expect_equal(as_tibble(dt2), tibble(x = c(1, 2), y = c(1, 3)))
})

test_that("slicing doesn't sorts groups", {
  dt <- lazy_dt(data.table(x = 2:1))
  expect_equal(
    dt %>% group_by(x) %>% slice(1) %>% pull(x),
    2:1
  )
})

test_that("doesn't return excess rows, #10", {
  dt <- lazy_dt(data.table(x = 1:2))
  expect_equal(
    dt %>% slice(1:3) %>% pull(x),
    1:2
  )
})

# variants ----------------------------------------------------------------

test_that("functions silently truncate results", {
  dt <- lazy_dt(data.frame(x = 1:5))

  expect_equal(dt %>% slice_head(n = 6) %>% as_tibble() %>% nrow(), 5)
  expect_equal(dt %>% slice_tail(n = 6) %>% as_tibble() %>% nrow(), 5)
  expect_equal(dt %>% slice_sample(n = 6) %>% as_tibble() %>% nrow(), 5)
  expect_equal(dt %>% slice_min(x, n = 6) %>% as_tibble() %>% nrow(), 5)
  expect_equal(dt %>% slice_max(x, n = 6) %>% as_tibble() %>% nrow(), 5)
  expect_equal(dt %>% slice_head(n = -6) %>% as_tibble() %>% nrow(), 0)
  expect_equal(dt %>% slice_tail(n = -6) %>% as_tibble() %>% nrow(), 0)
  expect_equal(dt %>% slice_sample(n = -6) %>% as_tibble() %>% nrow(), 0)
  expect_equal(dt %>% slice_min(x, n = -6) %>% as_tibble() %>% nrow(), 0)
  expect_equal(dt %>% slice_max(x, n = -6) %>% as_tibble() %>% nrow(), 0)
})

test_that("proportion rounds down", {
  dt <- lazy_dt(data.frame(x = 1:10))

  expect_equal(dt %>% slice_head(prop = 0.11) %>% as_tibble() %>% nrow(), 1)
  expect_equal(dt %>% slice_tail(prop = 0.11) %>% as_tibble() %>% nrow(), 1)
  expect_equal(dt %>% slice_sample(prop = 0.11) %>% as_tibble() %>% nrow(), 1)
  expect_equal(dt %>% slice_min(x, prop = 0.11) %>% as_tibble() %>% nrow(), 1)
  expect_equal(dt %>% slice_max(x, prop = 0.11) %>% as_tibble() %>% nrow(), 1)
  expect_equal(dt %>% slice_min(x, prop = 0.11, with_ties = FALSE) %>% as_tibble() %>% nrow(), 1)
  expect_equal(dt %>% slice_max(x, prop = 0.11, with_ties = FALSE) %>% as_tibble() %>% nrow(), 1)
})

test_that("min and max return ties by default", {
  dt <- lazy_dt(data.frame(x = c(1, 1, 1, 2, 2)))
  expect_equal(dt %>% slice_min(x) %>% collect() %>% nrow(), 3)
  expect_equal(dt %>% slice_max(x) %>% collect() %>% nrow(), 2)

  expect_equal(dt %>% slice_min(x, with_ties = FALSE) %>% collect() %>%  nrow(), 1)
  expect_equal(dt %>% slice_max(x, with_ties = FALSE) %>% collect() %>% nrow(), 1)
})

test_that("min and max work with character", {
  dt <- lazy_dt(data.table(x = c("b", "a", "d", "c")))
  expect_equal(dt %>% slice_min(x) %>% pull(x), "a")
  expect_equal(dt %>% slice_max(x) %>% pull(x), "d")
})

test_that("min and max reorder results and auto-convert data.tables", {
  dt <- lazy_dt(data.frame(id = 1:4, x = c(2, 3, 1, 2)))

  expect_equal(dt %>% slice_min(x, n = 2) %>% pull(id), c(3, 1, 4))
  expect_equal(dt %>% slice_min(x, n = 2, with_ties = FALSE) %>% pull(id), c(3, 1))
  expect_equal(dt %>% slice_max(x, n = 2) %>% pull(id), c(2, 1, 4))
  expect_equal(dt %>% slice_max(x, n = 2, with_ties = FALSE) %>% pull(id), c(2, 1))

  dt <- data.table(id = 1:4, x = c(2, 3, 1, 2))

  expect_equal(dt %>% slice_min(x, n = 2) %>% pull(id), c(3, 1, 4))
  expect_equal(dt %>% slice_max(x, n = 2) %>% pull(id), c(2, 1, 4))
})

test_that("min and max ignore NA's (#4826)", {
  dt <- lazy_dt(data.frame(id = 1:4, x = c(2, NA, 1, 2), y = c(NA, NA, NA, NA)))

  expect_equal(dt %>% slice_min(x, n = 2) %>% pull(id), c(3, 1, 4))
  expect_equal(dt %>% slice_min(y, n = 2) %>% pull(id), integer())
  expect_equal(dt %>% slice_max(x, n = 2) %>% pull(id), c(1, 4))
  expect_equal(dt %>% slice_max(y, n = 2) %>% pull(id), integer())
})

test_that("arguments to sample are passed along", {
  dt <- lazy_dt(data.frame(x = 1:100, wt = c(1, rep(0, 99))))

  expect_equal(dt %>% slice_sample(n = 1, weight_by = wt) %>% pull(x), 1)
  expect_equal(dt %>% slice_sample(n = 2, weight_by = wt, replace = TRUE) %>% pull(x), c(1, 1))
})

test_that("slice_*() checks for empty ...", {
  dt <- lazy_dt(data.frame(x = 1:10))
  expect_error(slice_head(dt, 5), class = "rlib_error_dots_nonempty")
  expect_error(slice_tail(dt, 5), class = "rlib_error_dots_nonempty")
  expect_error(slice_min(dt, x, 5), class = "rlib_error_dots_nonempty")
  expect_error(slice_max(dt, x, 5), class = "rlib_error_dots_nonempty")
  expect_error(slice_sample(dt, 5), class = "rlib_error_dots_nonempty")

  expect_error(slice_min(dt), "missing")
  expect_error(slice_max(dt), "missing")
})

test_that("slice_*() checks for constant n= and prop=", {
  dt <- lazy_dt(data.frame(x = 1:10))
  expect_error(slice_head(dt, n = n()), "constant")
  expect_error(slice_head(dt, prop = n()), "constant")
  expect_error(slice_tail(dt, n = n()), "constant")
  expect_error(slice_tail(dt, prop = n()), "constant")
  expect_error(slice_min(dt, x, n = n()), "constant")
  expect_error(slice_min(dt, x, prop = n()), "constant")
  expect_error(slice_max(dt, x, n = n()), "constant")
  expect_error(slice_max(dt, x, prop = n()), "constant")
  expect_error(slice_sample(dt, n = n()), "constant")
  expect_error(slice_sample(dt, prop = n()), "constant")
})

test_that("check_slice_catches common errors", {
  expect_snapshot(error = TRUE, {
    check_slice_size(n = 1, prop = 1)
    check_slice_size(n = "a")
    check_slice_size(prop = "a")
    check_slice_size(n = NA)
    check_slice_size(prop = NA)
  })
})

test_that("slice_head/slice_tail correctly slice ungrouped dt when n < 0", {
  dt <- lazy_dt(data.frame(x = 1:10))

  expect_equal(
    slice_head(dt, n = -2) %>% as_tibble(),
    slice_head(dt, n = nrow(dt) - 2) %>% as_tibble()
  )
  expect_equal(
    slice_tail(dt, n = -2) %>% as_tibble(),
    slice_tail(dt, n = nrow(dt) - 2) %>% as_tibble()
  )
})

test_that("slice_head/slice_tail correctly slice grouped dt when n < 0", {
  dt <-
    data.frame(x = 1:10, g = c(rep(1, 8), rep(2, 2))) %>%
    lazy_dt() %>%
    group_by(g)

  expect_equal(
    slice_head(dt, n = -3) %>% as_tibble(),
    slice(dt, rlang::seq2(1L, n() - 3)) %>% as_tibble()
  )
  expect_equal(
    n_groups(slice_head(dt, n = -3)),
    1L
  )
  expect_equal(
    slice_tail(dt, n = -3) %>% as_tibble(),
    slice(dt, rlang::seq2(3 + 1, n())) %>% as_tibble()
  )
  expect_equal(
    n_groups(slice_tail(dt, n = -3)),
    1L
  )

})

test_that("Non-integer number of rows computed correctly", {
  expect_equal(eval_tidy(get_slice_size(n = 1.6), list(.N = 10)), 1)
  expect_equal(eval_tidy(get_slice_size(prop = 0.16), list(.N = 10)), 1)
  expect_equal(eval_tidy(get_slice_size(n = -1.6), list(.N = 10)), 9)
  expect_equal(eval_tidy(get_slice_size(prop = -0.16), list(.N = 10)), 9)
})


# sample ------------------------------------------------------------------

test_that("basic usage generates expected calls", {
  dt <- lazy_dt(data.table(x = 1:5, y = 1), "DT")

  expect_equal(
    dt %>% sample_n(3) %>% show_query(),
    expr(DT[sample(.N, 3)])
  )
  expect_equal(
    dt %>% sample_frac(0.5) %>% show_query(),
    expr(DT[sample(.N, .N * 0.5)])
  )

  expect_equal(
    dt %>% sample_n(3, replace = TRUE) %>% show_query(),
    expr(DT[sample(.N, 3, replace = TRUE)])
  )
  expect_equal(
    dt %>% sample_n(3, weight = y) %>% show_query(),
    expr(DT[sample(.N, 3, prob = y)])
  )
})
