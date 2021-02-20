
# head and tail -------------------------------------------------------------

test_that("simple calls generate expected results", {
  dt <- lazy_dt(data.table(x = 1), "DT")

  expect_equal(
    dt %>% head() %>% show_query(),
    expr(head(DT, n = 6L))
  )
  expect_equal(
    dt %>% tail() %>% show_query(),
    expr(tail(DT, n = 6L))
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% head() %>% .$vars, c("x", "y"))
})


# rename ------------------------------------------------------------------

test_that("simple calls generate expected translations", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% rename(b = y) %>% show_query(),
    expr(setnames(copy(DT), "y", "b"))
  )
})

test_that("vars set correctly", {
  dt <- lazy_dt(data.frame(x = 1:3, y = 1:3))
  expect_equal(dt %>% rename(a = x) %>% .$vars, c("a", "y"))
})

test_that("empty rename returns original", {
  dt <- data.table(x = 1, y = 1, z = 1)
  lz <- lazy_dt(dt, "DT")

  expect_equal(lz %>% rename() %>% show_query(), expr(DT))
})

test_that("renames grouping vars", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1))
  gt <- group_by(dt, x)
  expect_equal(rename(gt, a = x)$groups, "a")
})

test_that("can rename with a function or formula", {
  dt <- lazy_dt(data.table(x = 1, y = 1))

  expect_equal(dt %>% rename_with(toupper) %>% .$vars, c("X", "Y"))
  expect_equal(dt %>% rename_with(toupper, 1) %>% .$vars, c("X", "y"))

  expect_equal(dt %>% rename_with("toupper") %>% .$vars, c("X", "Y"))
  expect_equal(dt %>% rename_with(~ toupper(.x)) %>% .$vars, c("X", "Y"))
})

test_that("but not with anything else", {
  dt <- lazy_dt(data.table(x = 1, y = 1))

  expect_snapshot(error = TRUE, {
    dt %>% rename_with(1)
  })
})

test_that("rename_with generates minimal spec", {
  dt <- lazy_dt(matrix(ncol = 26, dimnames = list(NULL, letters)), "DT")
  expect_snapshot({
    dt %>% rename_with(toupper) %>% show_query()
    dt %>% rename_with(toupper, 1:3) %>% show_query()
  })
})

test_that("can rename_with() a data.table", {
  dt <- data.table(x = 1:5, y = 1:5)
  out <- rename_with(dt, toupper, x)
  expect_s3_class(out, "dtplyr_step")
  expect_named(as_tibble(out), c("X", "y"))
})

# distinct ----------------------------------------------------------------

test_that("no input uses all variables", {
  dt <- lazy_dt(data.table(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct() %>% show_query(),
    expr(unique(dt))
  )

  expect_equal(dt %>% distinct() %>% .$vars, c("x", "y"))
})

test_that("uses supplied variables", {
  dt <- lazy_dt(data.table(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(y) %>% show_query(),
    expr(unique(dt[, .(y)]))
  )
  expect_equal(dt %>% distinct(y) %>% .$vars, "y")

  expect_equal(
    dt %>% group_by(x) %>% distinct(x, y) %>% show_query(),
    expr(unique(dt[, .(x, y)]))
  )
})

test_that("doesn't duplicate variables", {
  dt <- lazy_dt(data.table(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(x, x) %>% show_query(),
    expr(unique(dt[, .(x)]))
  )

  expect_equal(dt %>% distinct(x, x) %>% .$vars, "x")

  expect_equal(
    dt %>% group_by(x) %>% distinct(x) %>% show_query(),
    expr(unique(dt[, .(x)]))
  )
})
test_that("keeps all variables if requested", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "dt")

  expect_equal(
    dt %>% distinct(y, .keep_all = TRUE) %>% show_query(),
    expr(unique(dt, by = "y"))
  )
  expect_equal(dt %>% distinct(y, .keep_all = TRUE) %>% .$vars, c("x", "y", "z"))

  expect_equal(
    dt %>% group_by(x) %>% distinct(y, .keep_all = TRUE) %>% show_query(),
    expr(unique(dt, by = !!c("x", "y")))
  )
})

test_that("can compute distinct computed variables", {
  dt <- lazy_dt(data.table(x = c(1, 1), y = c(1, 2)), "dt")

  expect_equal(
    dt %>% distinct(z = x + y) %>% show_query(),
    expr(unique(dt[, .(z = x + y)]))
  )

  expect_equal(
    dt %>% distinct(z = x + y, .keep_all = TRUE) %>% show_query(),
    expr(unique(copy(dt)[, `:=`(z = x + y)], by = "z"))
  )
})


# unique ------------------------------------------------------------------

test_that("unique is an alias for distinct", {
  dt <- lazy_dt(data.table(x = c(1, 1)))
  expect_equal(unique(dt), distinct(dt))
})

# drop_na ------------------------------------------------------------------

test_that("empty call drops every row", {
  df <- lazy_dt(tibble(x = c(1, 2, NA), y = c("a", NA, "b")), "DT")
  exp <- tibble(x = c(1), y = c("a"))
  step <- drop_na(df)
  res <- as_tibble(step)
  expect_equal(show_query(step), expr(na.omit(DT)))
  expect_equal(res, exp)
})

test_that("converts data.table to dtplyr_step", {
  df <- data.table(x = c(1, 2, NA), y = c("a", NA, "b"))
  expect_equal(class(drop_na(df)), c("dtplyr_step_call", "dtplyr_step"))
})

test_that("specifying (a) variables considers only that variable(s)", {
  df <- lazy_dt(tibble(x = c(1, 2, NA), y = c("a", NA, "b")), "DT")
  exp <- tibble(x = c(1, 2), y = c("a", NA))
  step <- drop_na(df, x)
  res <- as_tibble(step)
  expect_equal(show_query(step), expr(na.omit(DT, cols = "x")))
  expect_equal(res, exp)
  exp <- tibble(x = c(1), y = c("a"))
  res <- as_tibble(drop_na(df, x:y))
  expect_equal(res, exp)
})

test_that("groups are preserved", {
  df <- lazy_dt(tibble(g = c("A", "A", "B"), x = c(1, 2, NA), y = c("a", NA, "b")))
  exp <- tibble(g = c("A", "B"), x = c(1, NA), y = c("a", "b"))

  gdf <- group_by(df, "g")
  gexp <- dplyr::group_by(exp, "g")

  res <- collect(drop_na(gdf, y))
  expect_equal(res, gexp)
  expect_equal(dplyr::group_vars(res), dplyr::group_vars(gexp))
})

test_that("empty call drops every row", {
  df <- lazy_dt(tibble(x = c(1, 2, NA), y = c("a", NA, "b")))
  res <- as_tibble(drop_na(df))
  expect_identical(res, tibble(x = 1, y = "a"))
})

test_that("errors are raised", {
  df <- lazy_dt(tibble(x = c(1, 2, NA), y = c("a", NA, "b")))
  expect_error(collect(drop_na(df, !! list())))
  expect_error(collect(drop_na(df, "z")))
})

test_that("single variable data.frame doesn't lose dimension", {
  df <- lazy_dt(data.frame(x = c(1, 2, NA)))
  res <- as.data.frame(drop_na(df, "x"))
  exp <- data.frame(x = c(1, 2))
  expect_equal(res, exp)
})

test_that("preserves attributes", {
  df <- lazy_dt(tibble(x = structure(c(1, NA), attr = "!")))
  rs <- collect(drop_na(df))

  expect_equal(rs$x, structure(1, attr = "!"))
})
