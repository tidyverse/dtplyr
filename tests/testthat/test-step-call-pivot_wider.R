test_that("can pivot all cols to wide", {
  df <- lazy_dt(tibble(key = c("x", "y", "z"), val = 1:3), "DT")
  step <- pivot_wider(df, names_from = key, values_from = val)
  pv <- collect(step)

  expect_equal(step$vars, c("x", "y", "z"))
  expect_equal(nrow(pv), 1)
  expect_equal(
    show_query(step),
    expr(dcast(DT, formula = "..." ~ key, value.var = "val")[, .(x, y, z)])
  )
})

test_that("non-pivoted cols are preserved", {
  df <- lazy_dt(tibble(a = 1, key = c("x", "y"), val = 1:2), "DT")
  step <- pivot_wider(df, names_from = key, values_from = val)
  pv <- collect(step)

  expect_equal(step$vars, c("a", "x", "y"))
  expect_equal(nrow(pv), 1)
  expect_equal(
    show_query(step),
    expr(dcast(DT, formula = a ~ key, value.var = "val"))
  )
})

test_that("implicit missings turn into explicit missings", {
  df <- lazy_dt(tibble(a = 1:2, key = c("x", "y"), val = 1:2))
  pv <- collect(pivot_wider(df, names_from = key, values_from = val))

  expect_equal(pv$a, c(1, 2))
  expect_equal(pv$x, c(1, NA))
  expect_equal(pv$y, c(NA, 2))
})

test_that("error when overwriting existing column", {
  df <- tibble(
    a = c(1, 1),
    key = c("a", "b"),
    val = c(1, 2)
  )
  df <- lazy_dt(df)
  expect_error(
    pivot_wider(df, names_from = key, values_from = val),
    "Names must be unique"
  )
})

test_that("grouping is preserved", {
  df <- lazy_dt(tibble(g = 1, k = "x", v = 2))
  out <- df %>%
    group_by(g) %>%
    pivot_wider(names_from = k, values_from = v)
  expect_equal(out$groups, "g")
})

# https://github.com/tidyverse/tidyr/issues/804
test_that("column with `...j` name can be used as `names_from`", {
  df <- lazy_dt(tibble(...8 = c("x", "y", "z"), val = 1:3))
  step <- pivot_wider(df, names_from = ...8, values_from = val)
  pv <- collect(step)
  expect_equal(step$vars, c("x", "y", "z"))
  expect_equal(nrow(pv), 1)
})

# column names -------------------------------------------------------------

test_that("names_glue affects output names & auto-converts data.table to lazy_dt", {
  df <- lazy_dt(
    data.frame(
      x = c("X", "Y"),
      y = 1:2,
      a = 1:2,
      b = 1:2
    ),
    "DT"
  )

  step <- pivot_wider(df, names_from = x:y, values_from = a:b, names_glue = "{x}{y}_{.value}")

  expect_snapshot(show_query(step))
  expect_equal(step$vars, c("X1_a", "Y2_a", "X1_b", "Y2_b"))
})

test_that("can use names_glue without .value", {
  df <- lazy_dt(tibble(label = c("x", "y", "z"), val = 1:3))
  step <- pivot_wider(
    df, names_from = label, values_from = val, names_glue = "test_{label}"
  )
  pv <- collect(step)

  expect_equal(step$vars, c("test_x", "test_y", "test_z"))
  expect_equal(nrow(pv), 1)
})

test_that("can add name prefix", {
  df <- lazy_dt(tibble(label = c("x", "y", "z"), val = 1:3), "DT")
  step <- pivot_wider(
    df, names_from = label, values_from = val, names_prefix = "test_"
  )
  expect_named(collect(step), c("test_x", "test_y", "test_z"))
})

test_that("can sort column names", {
  df <- tibble(
    int = c(1, 3, 2),
    chr = c("Wed", "Tue", "Mon"),
  )
  df <- lazy_dt(df, "DT")
  step <- pivot_wider(df, names_from = chr, values_from = int, names_sort = TRUE)

  expect_snapshot(show_query(step))
  expect_equal(step$vars, c("Mon", "Tue", "Wed"))
})

test_that("can sort column names with id", {
  df <- tibble(
    id = 1:3,
    int = c(1, 3, 2),
    chr = c("Wed", "Tue", "Mon"),
  )
  df <- lazy_dt(df, "DT")
  step <- pivot_wider(df, names_from = chr, values_from = int, names_sort = TRUE)

  expect_snapshot(show_query(step))
  expect_equal(step$vars, c("id", "Mon", "Tue", "Wed"))
})

test_that("can repair names if requested", {
  df <- lazy_dt(tibble(x = 1, lab = "x", val = 2), "DT")
  expect_snapshot(error = TRUE, {
    pivot_wider(df, names_from = lab, values_from = val)
    pivot_wider(df, names_from = lab, values_from = val, names_repair = "unique")
  })
})

test_that("can handle numeric column in names_from", {
  df <- lazy_dt(tibble(x = 1, name = 1, value = 2), "DT")
  expect_named(pivot_wider(df, names_prefix = "nm") %>% collect(), c("x", "nm1"))
})

# keys ---------------------------------------------------------

test_that("can override default keys", {
  df <- tribble(
    ~row, ~name, ~var, ~value,
    1,    "Sam", "age", 10,
    2,    "Sam", "height", 1.5,
    3,    "Bob", "age", 20,
  )
  df <- lazy_dt(df, "DT")
  step <- pivot_wider(df, id_cols = name, names_from = var, values_from = value)
  pv <- collect(step)

  expect_equal(nrow(pv), 2)
  expect_equal(
    show_query(step),
    expr(dcast(DT, formula = name ~ var, value.var = "value"))
  )
})


# non-unique keys ---------------------------------------------------------

test_that("warning suppressed by supplying values_fn", {
  df <- lazy_dt(tibble(a = c(1, 1, 2), key = c("x", "x", "x"), val = 1:3))

  pv <- df %>%
    pivot_wider(names_from = key,
                values_from = val,
                values_fn = list(val = list)) %>%
    collect()

  expect_equal(pv$a, c(1, 2))
  expect_equal(as.list(pv$x), list(c(1L, 2L), 3L))
})

test_that("values_fn can be a single function", {
  df <- lazy_dt(tibble(a = c(1, 1, 2), key = c("x", "x", "x"), val = c(1, 10, 100)), "DT")
  step <- pivot_wider(df, names_from = key, values_from = val, values_fn = sum)
  pv <- collect(step)

  expect_equal(step$vars, c("a", "x"))
  expect_equal(pv$x, c(11, 100))
})

test_that("values_summarize applied even when no-duplicates", {
  df <- lazy_dt(tibble(a = c(1, 2), key = c("x", "x"), val = 1:2))
  pv <- df %>%
    pivot_wider(names_from = key,
                values_from = val,
                values_fn = list(val = list)) %>%
    collect()

  expect_equal(pv$a, c(1, 2))
  expect_equal(as.list(pv$x), list(1L, 2L))
})


# can fill missing cells --------------------------------------------------

test_that("can fill in missing cells", {
  df <- lazy_dt(tibble(g = c(1, 2), var = c("x", "y"), val = c(1, 2)))

  widen <- function(...) {
    df %>%
      pivot_wider(names_from = var, values_from = val, ...) %>%
      collect()
  }

  expect_equal(widen()$x, c(1, NA))
  expect_equal(widen(values_fill = 0)$x, c(1, 0))
  expect_equal(widen(values_fill = list(val = 0))$x, c(1, 0))
})

test_that("values_fill only affects missing cells", {
  df <- lazy_dt(tibble(g = c(1, 2), names = c("x", "y"), value = c(1, NA)), "DT")
  step <- pivot_wider(df, names_from = names, values_from = value, values_fill = 0)
  out <- collect(step)

  expect_equal(out$y, c(0, NA))
  expect_equal(
    show_query(step),
    expr(dcast(DT, formula = g ~ names, value.var = "value", fill = 0))
  )
})

# multiple values ----------------------------------------------------------

test_that("can pivot from multiple measure cols", {
  df <- lazy_dt(tibble(row = 1, var = c("x", "y"), a = 1:2, b = 3:4))
  step <- pivot_wider(df, names_from = var, values_from = c(a, b))
  pv <- collect(step)

  expect_equal(step$vars, c("row", "a_x", "a_y", "b_x", "b_y"))
  expect_equal(pv$a_x, 1)
  expect_equal(pv$b_y, 4)
})

test_that("can pivot from multiple measure cols using all keys", {
  df <- lazy_dt(tibble(var = c("x", "y"), a = 1:2, b = 3:4))
  step <- pivot_wider(df, names_from = var, values_from = c(a, b))
  pv <- collect(step)

  expect_equal(step$vars, c("a_x", "a_y", "b_x", "b_y"))
  expect_equal(pv$a_x, 1)
  expect_equal(pv$b_y, 4)
})
