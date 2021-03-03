test_that("constructor has sensible defaults", {
  dt <- data.table(x = 1:2, y = 1:2)
  step <- step_first(dt)

  expect_s3_class(step, "dtplyr_step_first")
  expect_equal(step$parent, dt)
  expect_equal(step$vars, c("x", "y"))
  expect_equal(step$groups, character())
  expect_match(as.character(step$name), "_DT")
})


# mutability --------------------------------------------------------------

test_that("doesn't need copy", {
  dt <- lazy_dt(mtcars)
  expect_false(dt$needs_copy)
})

test_that("mutable object must be a data table", {
  expect_error(lazy_dt(mtcars, immutable = FALSE), "not already a data table")
})

test_that("mutable object never needs copy", {
  dt <- lazy_dt(as.data.table(mtcars), immutable = FALSE)
  expect_false(dt$needs_copy)
  expect_false(dt %>% mutate(x = 1) %>% .$needs_copy)
})

test_that("dt_call() copies if requested", {
  dt <- lazy_dt(mtcars, name = "DT")

  expect_equal(dt_call(dt, FALSE), quote(DT))
  expect_equal(dt_call(dt, TRUE), quote(copy(DT)))
})

test_that("lazy_dt doesn't copy input", {
  dt <- data.table(x = 1)
  lz <- lazy_dt(dt)

  expect_equal(data.table::address(dt), data.table::address(lz$parent))
})

# keys --------------------------------------------------------------------

test_that("can set keys", {
  dt <- lazy_dt(mtcars, key_by = cyl)
  expect_equal(data.table::key(dt$parent), "cyl")
})

test_that("setting doesn't modify data.table", {
  dt1 <- data.table(x = c(5, 1, 2))
  dt2 <- lazy_dt(dt1, key_by = x)

  expect_equal(data.table::key(dt1$parent), NULL)
  expect_equal(data.table::key(dt2$parent), "x")
})

# groups ------------------------------------------------------------------

test_that("keeps groups", {
  dt <- lazy_dt(group_by(mtcars, cyl))
  expect_equal(group_vars(dt), "cyl")
})
