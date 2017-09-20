context("select")

test_that("adds grouping variables", {
  res <- mtcars %>% tbl_dt() %>% group_by(vs) %>% select(mpg)
  expect_named(res, c("vs", "mpg"))
})

test_that("select changes columns in copy of data table", {
  dt <- data.table::data.table(x = 1:4, y = letters[1:4])

  expect_equal(names(select(dt, x, z = y)), c("x", "z"))
  expect_equal(names(dt), c("x", "y"))

  gdt <- dt %>% group_by(x)
  expect_equal(names(select(gdt, x, z = y)), c("x", "z"))
  expect_equal(names(gdt), c("x", "y"))
})


context("select (tests from dplyr):")

mtcars_dt <- as.data.table(mtcars)

expect_groups <- function(df, groups, info = NULL) {
  if (length(groups) == 0L) {
    expect_null(groups(df), info = info)
    expect_identical(group_vars(df), character(), info = info)
  } else {
    expect_identical(groups(df), lapply(enc2native(groups), as.name), info = info)
    expect_identical(group_vars(df), groups, info = info)
  }
}

test_that("select does not lose grouping (#147)", {
  df <- data.table(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8))
  grouped <- df %>% group_by(a) %>% select(a, b, x)

  expect_groups(grouped, "a")
})

test_that("grouping variables preserved with a message (#1511)", {
  df <- data.table(g = 1:3, x = 3:1) %>% group_by(g)

  expect_message(res <- select(df, x), "Adding missing grouping variables")
  expect_named(res, c("g", "x"))
})

test_that("non-syntactic grouping variable is preserved (#1138)", {
  df <- data.table(`a b` = 1L) %>% group_by(`a b`) %>% select()
  expect_named(df, "a b")
})

test_that("select doesn't fail if some names missing", {
  df1 <- data.frame(x = 1:10, y = 1:10, z = 1:10)
  df2 <- setNames(df1, c("x", "y", ""))
  # df3 <- setNames(df1, c("x", "", ""))

  expect_equal(select(df1, x), data.frame(x = 1:10))
  expect_equal(select(df2, x), data.frame(x = 1:10))
  # expect_equal(select(df3, x), data.frame(x = 1:10))
})


# Empty selects -------------------------------------------------

test_that("select with no args returns nothing", {
  skip_if_dtplyr()

  empty <- select(mtcars_dt)
  expect_equal(ncol(empty), 0)
  expect_equal(nrow(empty), 32)
})

test_that("select excluding all vars returns nothing", {
  skip_if_dtplyr()

  expect_equal(dim(select(mtcars_dt, -(mpg:carb))), c(32, 0))
  expect_equal(dim(select(mtcars_dt, starts_with("x"))), c(32, 0))
  expect_equal(dim(select(mtcars_dt, -matches("."))), c(32, 0))

  expect_equal(dim(select(mtcars_dt, -(mpg:carb))), c(0, 0))
  expect_equal(dim(select(mtcars_dt, starts_with("x"))), c(0, 0))
  expect_equal(dim(select(mtcars_dt, -matches("."))), c(0, 0))
})

test_that("negating empty match returns everything", {
  df <- data.frame(x = 1:3, y = 3:1)
  expect_equal(select(df, -starts_with("xyz")), df)
})

# Select variables -----------------------------------------------

test_that("select_vars can rename variables", {
  vars <- c("a", "b")
  expect_equal(select_vars(vars, b = a, a = b), c("b" = "a", "a" = "b"))
})

test_that("last rename wins", {
  vars <- c("a", "b")

  expect_equal(select_vars(vars, b = a, c = a), c("c" = "a"))
})

test_that("negative index removes values", {
  vars <- letters[1:3]

  expect_equal(select_vars(vars, -c), c(a = "a", b = "b"))
  expect_equal(select_vars(vars, a:c, -c), c(a = "a", b = "b"))
  expect_equal(select_vars(vars, a, b, c, -c), c(a = "a", b = "b"))
  expect_equal(select_vars(vars, -c, a, b), c(a = "a", b = "b"))
})

test_that("select can be before group_by (#309)", {
  df <- data.frame(
    id = c(1, 1, 2, 2, 2, 3, 3, 4, 4, 5),
    year = c(2013, 2013, 2012, 2013, 2013, 2013, 2012, 2012, 2013, 2013),
    var1 = rnorm(10)
  )
  dfagg <- df %>%
    group_by(id, year) %>%
    select(id, year, var1) %>%
    summarise(var1 = mean(var1))
  expect_equal(names(dfagg), c("id", "year", "var1"))
  expect_equal(attr(dfagg, "vars"), "id")

})

test_that("rename does not crash with invalid grouped data frame (#640)", {
  df <- data.table(a = 1:3, b = 2:4, d = 3:5) %>% group_by(a, b)
  df$a <- NULL
  expect_equal(
    df %>% rename(e = d) %>% ungroup,
    data.table(b = 2:4, e = 3:5)
  )
  expect_equal(
    df %>% rename(e = b) %>% ungroup,
    data.table(e = 2:4, d = 3:5)
  )
})

test_that("can select with character vectors", {
  skip_if_dtplyr()

  expect_identical(select_vars(letters, "b", !! "z", c("b", "c")), set_names(c("b", "z", "c")))
})

test_that("abort on unknown columns", {
  skip_if_dtplyr()

  expect_error(select_vars(letters, "foo"), "must match column names")
  expect_error(select_vars(letters, c("a", "bar", "foo", "d")), "bar, foo")
})

test_that("rename() handles data pronoun", {
  expect_identical(rename(data.table(x = 1), y = .data$x), data.table(y = 1))
})


# combine_vars ------------------------------------------------------------
# This is the low C++ function which works on integer indices

test_that("empty index gives empty output", {
  skip_if_dtplyr()

  vars <- combine_vars(letters, list())
  expect_equal(length(vars), 0)

  vars <- combine_vars(letters, list(numeric()))
  expect_equal(length(vars), 0)
})

test_that("positive indexes kept", {
  skip_if_dtplyr()

  expect_equal(combine_vars(letters, list(1)), c(a = 1))
  expect_equal(combine_vars(letters, list(1, 26)), c(a = 1, z = 26))
  expect_equal(combine_vars(letters, list(c(1, 26))), c(a = 1, z = 26))
})

test_that("indexes returned in order they appear", {
  skip_if_dtplyr()

  expect_equal(combine_vars(letters, list(26, 1)), c(z = 26, a = 1))
})


test_that("negative index in first position includes all others", {
  skip_if_dtplyr()

  vars <- combine_vars(letters[1:3], list(-1))
  expect_equal(vars, c(b = 2, c = 3))
})

test_that("named inputs rename outputs", {
  skip_if_dtplyr()

  expect_equal(combine_vars(letters[1:3], list(d = 1)), c(d = 1))
  expect_equal(combine_vars(letters[1:3], list(c(d = 1))), c(d = 1))
})

test_that("if multiple names, last kept", {
  skip_if_dtplyr()

  expect_equal(combine_vars(letters[1:3], list(d = 1, e = 1)), c(e = 1))
  expect_equal(combine_vars(letters[1:3], list(c(d = 1, e = 1))), c(e = 1))
})

test_that("if one name for multiple vars, use integer index", {
  skip_if_dtplyr()

  expect_equal(combine_vars(letters[1:3], list(x = 1:3)), c(x1 = 1, x2 = 2, x3 = 3))
})

test_that("invalid inputs raise error", {
  skip_if_dtplyr()

  expect_error(
    combine_vars(names(mtcars_dt), list(0)),
    "Each argument must yield either positive or negative integers",
    fixed = TRUE
  )
  expect_error(
    combine_vars(names(mtcars_dt), list(c(-1, 1))),
    "Each argument must yield either positive or negative integers",
    fixed = TRUE
  )
  expect_error(
    combine_vars(names(mtcars_dt), list(12)),
    "Position must be between 0 and n",
    fixed = TRUE
  )
})

test_that("select succeeds in presence of raw columns (#1803)", {
  skip_if_dtplyr()
  
  df <- data.table(a = 1:3, b = as.raw(1:3))
  expect_identical(select(df, a), df["a"])
  expect_identical(select(df, b), df["b"])
  expect_identical(select(df, -b), df["a"])
})

test_that("arguments to select() don't match select_vars() arguments", {
  skip_if_dtplyr()

  df <- data.table(a = 1)
  expect_identical(select(df, var = a), data.table(var = 1))
  expect_identical(select(group_by(df, a), var = a), group_by(data.table(var = 1), var))
  expect_identical(select(df, exclude = a), data.table(exclude = 1))
  expect_identical(select(df, include = a), data.table(include = 1))
  expect_identical(select(group_by(df, a), exclude = a), group_by(data.table(exclude = 1), exclude))
  expect_identical(select(group_by(df, a), include = a), group_by(data.table(include = 1), include))
})

test_that("arguments to rename() don't match rename_vars() arguments (#2861)", {
  skip_if_dtplyr()

  df <- data.table(a = 1)
  expect_identical(rename(df, var = a), data.table(var = 1))
  expect_identical(rename(group_by(df, a), var = a), group_by(data.table(var = 1), var))
  expect_identical(rename(df, strict = a), data.table(strict = 1))
  expect_identical(rename(group_by(df, a), strict = a), group_by(data.table(strict = 1), strict))
})

test_that("can select() with .data pronoun (#2715)", {
  expect_identical(select(mtcars_dt, .data$cyl), select(mtcars_dt, cyl))
})

test_that("can select() with character vectors", {
  expect_identical(select(mtcars_dt, "cyl", !! "disp", c("cyl", "am", "drat")), mtcars_dt[, c("cyl", "disp", "am", "drat")])
})

test_that("rename() to UTF-8 column names", {
  skip_if_dtplyr()

  skip_on_os("windows") # needs an rlang update? #3049
  df <- data.table(a = 1) %>% rename("\u5e78" := a)

  expect_equal(colnames(df), "\u5e78")
})

