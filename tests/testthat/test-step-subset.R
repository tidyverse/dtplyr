test_that("construtor has sensible defaults", {
  first <- step_first(data.table(x = 1), "DT")
  step <- step_subset(first)

  expect_s3_class(step, "dtplyr_step_subset")
  expect_equal(step$parent, first)
  expect_equal(step$vars, "x")
  expect_equal(step$groups, character())
  expect_equal(step$i, NULL)
  expect_equal(step$j, NULL)
})

test_that("generates expected calls", {
  first <- lazy_dt(data.table(x = 1), "DT")

  ungrouped <- step_subset(first, i = quote(i), j = quote(j))
  expect_equal(dt_call(ungrouped), expr(DT[i, j]))

  with_i <- step_subset(first, i = quote(i), j = quote(j), groups = "x")
  expect_equal(dt_call(with_i), expr(DT[, .SD[i, j], by = .(x)]))

  without_i <- step_subset(first, j = quote(j), groups = "x")
  expect_equal(dt_call(without_i), expr(DT[, j, by = .(x)]))
})

# dplyr methods -----------------------------------------------------------

test_that("simple calls generate expected translations", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% select(-z) %>% show_query(),
    expr(DT[, .(x, y)])
  )

  expect_equal(
    dt %>% select(a = x, y) %>% show_query(),
    expr(DT[, .(a = x, y)])
  )

  expect_equal(
    dt %>% summarise(x = mean(x)) %>% show_query(),
    expr(DT[, .(x = mean(x))])
  )

  expect_equal(
    dt %>% arrange(x) %>% show_query(),
    expr(DT[order(x), ])
  )

  expect_equal(
    dt %>% filter() %>% show_query(),
    expr(DT)
  )

  expect_equal(
    dt %>% filter(x > 1) %>% show_query(),
    expr(DT[x > 1, ])
  )

  expect_equal(
    dt %>% filter(x > 1, y > 2) %>% show_query(),
    expr(DT[x > 1 & y > 2, ])
  )
})

test_that("can merge iff j-generating call comes after i", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% filter(x > 1) %>% select(y) %>% show_query(),
    expr(DT[x > 1, .(y)])
  )
  expect_equal(
    dt %>% select(x = y) %>% filter(x > 1) %>% show_query(),
    expr(DT[, .(x = y)][x > 1, ])
  )

  expect_equal(
    dt %>% filter(x > 1) %>% summarise(y = mean(x)) %>% show_query(),
    expr(DT[x > 1, .(y = mean(x))])
  )
  expect_equal(
    dt %>% summarise(y = mean(x)) %>% filter(x > 1) %>% show_query(),
    expr(DT[, .(y = mean(x))][x > 1, ])
  )
})

# arrange -----------------------------------------------------------------

test_that("arrange doesn't use, but still preserves, grouping", {
  dt <- group_by(lazy_dt(data.table(x = 1, y = 2), "DT"), x)

  step <- arrange(dt, y)
  expect_equal(step$groups, "x")
  expect_equal(dt_call(step), expr(DT[order(y), ]))

  step2 <- arrange(dt, y, .by_group = TRUE)
  expect_equal(dt_call(step2), expr(DT[order(x, y), ]))
})

test_that("empty arrange returns input unchanged", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")
  expect_true(identical(arrange(dt), dt))
})

# summarise ---------------------------------------------------------------

test_that("summarise peels off layer of grouping", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1))
  gt <- group_by(dt, x, y)

  expect_equal(summarise(gt)$groups, "x")
  expect_equal(summarise(summarise(gt))$groups, character())
})

# select/rename ------------------------------------------------------------------

test_that("renames grouping vars", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1))
  gt <- group_by(dt, x)

  expect_equal(select(gt, y = x)$groups, "y")
})

test_that("empty select returns no columns", {
  dt <- data.table(x = 1, y = 1, z = 1)
  lz <- lazy_dt(dt, "DT")
  expect_equal(
    lz %>% select() %>% collect(),
    dt[, 0]
  )

  # unless it's grouped
  expect_equal(
    lz %>% group_by(x) %>% select() %>% collect(),
    dt[, "x"]
  )
})

# slice -------------------------------------------------------------------

test_that("can slice", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% slice() %>% show_query(),
    expr(DT)
  )
  expect_equal(
    dt %>% slice(1:4) %>% show_query(),
    expr(DT[1:4, ])
  )
  expect_equal(
    dt %>% slice(1, 2, 3) %>% show_query(),
    expr(DT[c(1, 2, 3), ])
  )
})

test_that("can slice when grouped", {
  dt <- lazy_dt(data.table(x = 1:4, y = c(1, 2, 1, 2)), "DT")

  expect_equal(
    dt %>% group_by(x) %>% slice(1) %>% show_query(),
    expr(DT[, .SD[1, ], by = .(x)])
  )
})
