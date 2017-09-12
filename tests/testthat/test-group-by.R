context("group_by")

test_that("original data table not modified by grouping", {
  dt <- data.table(x = 5:1)
  dt2 <- group_by(dt, x)
  dt2$y <- 1:5

  expect_equal(dt$x, 5:1)
  expect_equal(dt$y, NULL)
})

test_that("group_by with add = TRUE adds groups", {
  dt <- data.table(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

  add_groups1 <- function(tbl) groups(group_by(tbl, x, y, add = TRUE))
  add_groups2 <- function(tbl) groups(group_by(group_by(tbl, x, add = TRUE), y,
    add = TRUE))

  expect_equal(add_groups1(dt), list(quote(x), quote(y)))
  expect_equal(add_groups2(dt), list(quote(x), quote(y)))
})

test_that("data.table invalid .selfref issue (dplyr#475)", {
  dt <- data.table(x = 1:5, y = 6:10)
  expect_warning((dt %>% group_by(x))[, z := 2L], NA)
  dt <- data.table(x = 1:5, y = 6:10)
  expect_warning((dt %>% group_by(x) %>% summarise(z = y ^ 2))[, foo := 1L], NA)
})


test_that("constructors drops groups", {
  dt <- lahman_dt() %>% tbl("Batting") %>% group_by(playerID)
  expect_equal(groups(tbl_dt(dt)), NULL)
})


context("Group by (tests from dplyr)")

expect_groups <- function(df, groups, info = NULL) {
  if (length(groups) == 0L) {
    expect_null(groups(df), info = info)
    expect_identical(group_vars(df), character(), info = info)
  } else {
    expect_identical(groups(df), lapply(enc2native(groups), as.name), info = info)
    expect_identical(group_vars(df), groups, info = info)
  }
}

expect_no_groups <- function(df) {
  expect_groups(df, NULL)
}

df <- data.table(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

test_that("group_by with add = TRUE adds groups", {
  add_groups1 <- function(tbl) group_by(tbl, x, y, add = TRUE)
  add_groups2 <- function(tbl) group_by(group_by(tbl, x, add = TRUE), y, add = TRUE)

  expect_groups(add_groups1(df), c("x", "y"))
  expect_groups(add_groups2(df), c("x", "y"))
})

test_that("joins preserve grouping", {
  g <- group_by(df, x)

  expect_groups(inner_join(g, g, by = c("x", "y")), "x")
  expect_groups(left_join (g, g, by = c("x", "y")), "x")
  # expect_groups(semi_join (g, g, by = c("x", "y")), "x")
  expect_groups(anti_join (g, g, by = c("x", "y")), "x")
})

test_that("constructors drops groups", {
  df <- data.table(x = 1:3) %>% group_by(x)
  expect_no_groups(tbl_dt(df))
})

test_that("grouping by constant adds column (#410)", {
  grouped <- group_by(mtcars, "cyl") %>% summarise(foo = n())
  expect_equal(names(grouped), c('"cyl"', "foo"))
  expect_equal(nrow(grouped), 1L)
})

# Test full range of variable types --------------------------------------------


test_that("local group_by preserves variable types", {
  df_var <- data.table(
    l = c(T, F),
    i = 1:2,
    d = Sys.Date() + 1:2,
    f = factor(letters[1:2]),
    num = 1:2 + 0.5,
    t = Sys.time() + 1:2,
    c = letters[1:2]
  )

  for (var in names(df_var)) {
    expected <- data.table(unique(df_var[[var]]), n = 1L)
    names(expected)[1] <- var

    summarised <- df_var %>% group_by(!! rlang::sym(var)) %>% summarise(n = n())
    expect_equal(summarised, expected, info = var)
  }
})

test_that("mutate does not loose variables (#144)", {
  df <- tbl_dt(data.table(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8)))
  by_ab <- group_by(df, a, b)
  by_a  <- summarise(by_ab, x = sum(x))
  by_a_quartile <- group_by(by_a, quartile = ntile(x, 4))

  expect_equal(names(by_a_quartile), c("a", "b", "x", "quartile"))
})

# test_that("group_by uses shallow copy", {
#   m1 <- group_by(mtcars, cyl)
#   expect_no_groups(mtcars)

#   expect_equal(dfloc(mtcars), dfloc(m1))
# })

test_that("FactorVisitor handles NA. #183", {
  g <- group_by(as.data.table(MASS::survey), M.I)
  expect_equal(g$M.I, MASS::survey$M.I)
})

# test_that("group_by orders by groups. #242", {
#   df <- data.table(a = sample(1:10, 3000, replace = TRUE)) %>% group_by(a)
#   expect_equal(attr(df, "labels")$a, 1:10)

#   df <- data.table(a = sample(letters[1:10], 3000, replace = TRUE), stringsAsFactors = FALSE) %>% group_by(a)
#   expect_equal(attr(df, "labels")$a, letters[1:10])

#   df <- data.table(a = sample(sqrt(1:10), 3000, replace = TRUE)) %>% group_by(a)
#   expect_equal(attr(df, "labels")$a, sqrt(1:10))
# })

# test_that("group_by uses the white list", {
#   df <- data.table(times = 1:5)
#   df$times <- as.POSIXlt(seq.Date(Sys.Date(), length.out = 5, by = "day"))
#   expect_error(
#     group_by(df, times),
#     "Column `times` is of unsupported class POSIXlt/POSIXt",
#     fixed = TRUE
#   )
# })

# test_that("group_by fails when lists are used as grouping variables (#276)", {
#   df <- data.table(x = 1:3)
#   df$y <- list(1:2, 1:3, 1:4)
#   expect_error(
#     group_by(df, y),
#     "Column `y` can't be used as a grouping variable because it's a list",
#     fixed = TRUE
#   )
# })


test_that("select(group_by(.)) implicitely adds grouping variables (#170)", {
  res <- as.data.table(mtcars) %>% group_by(vs) %>% select(mpg)
  expect_equal(names(res), c("vs", "mpg"))
})

# test_that("grouped_df errors on empty vars (#398)", {
#   m <- as.data.table(mtcars) %>% group_by(cyl)
#   attr(m, "vars") <- NULL
#   attr(m, "indices") <- NULL
#   expect_error(
#     m %>% do(mpg = mean(.$mpg)),
#     "no variables to group by",
#     fixed = TRUE
#   )
# })

test_that("grouped_df errors on non-existent var (#2330)", {
  df <- data.table(x = 1:5)
  expect_error(
    grouped_df(df, list(quote(y))),
    "Column `y` is unknown"
  )
})

test_that("group_by only creates one group for NA (#401)", {
  x <- as.numeric(c(NA, NA, NA, 10:1, 10:1))
  w <- c(20, 30, 40, 1:10, 1:10) * 10

  n_distinct(x) # 11 OK
  res <- data.table(x = x, w = w) %>% group_by(x) %>% summarise(n = n())
  expect_equal(nrow(res), 11L)
})

test_that("there can be 0 groups (#486)", {
  data <- data.table(a = numeric(0), g = character(0)) %>% group_by(g)
  expect_equal(length(data$a), 0L)
  expect_equal(length(data$g), 0L)
  # expect_equal(attr(data, "group_sizes"), integer(0))
})

test_that("group_by works with zero-row data frames (#486)", {
  dfg <- group_by(data.table(a = numeric(0), b = numeric(0), g = character(0)), g)
  expect_equal(dim(dfg), c(0, 3))
  expect_groups(dfg, "g")
  expect_equal(group_size(dfg), integer(0))

  x <- summarise(dfg, n = n())
  expect_equal(dim(x), c(0, 2))
  expect_no_groups(x)

  x <- mutate(dfg, c = b + 1)
  expect_equal(dim(x), c(0, 4))
  expect_groups(x, "g")
  expect_equal(group_size(x), integer(0))

  x <- filter(dfg, a == 100)
  expect_equal(dim(x), c(0, 3))
  expect_groups(x, "g")
  expect_equal(group_size(x), integer(0))

  x <- arrange(dfg, a, g)
  expect_equal(dim(x), c(0, 3))
  # expect_groups(x, "g")
  # expect_equal(group_size(x), integer(0))

  x <- select(dfg, a)  # Only select 'a' column; should result in 'g' and 'a'
  expect_equal(dim(x), c(0, 2))
  expect_groups(x, "g")
  expect_equal(group_size(x), integer(0))
})

test_that("grouped_df requires a list of symbols (#665)", {
  features <- list("feat1", "feat2", "feat3")
  # error message by assertthat
  expect_error(grouped_df(data.table(feat1 = 1, feat2 = 2, feat3 = 3), features))
})

test_that("group_by gives meaningful message with unknow column (#716)", {
  expect_error(
    group_by(iris, wrong_name_of_variable),
    "Column `wrong_name_of_variable` is unknown",
    fixed = TRUE
  )
})

# test_that("[ on grouped_df preserves grouping if subset includes grouping vars", {
#   df <- data.table(x = 1:5, ` ` = 6:10)
#   by_x <- df %>% group_by(x)
#   expect_equal(by_x %>% groups(), by_x %>% `[`(1:2) %>% groups)

#   # non-syntactic name
#   by_ns <- df %>% group_by(` `)
#   expect_equal(by_ns %>% groups(), by_ns %>% `[`(1:2) %>% groups)
# })


test_that("[ on grouped_df drops grouping if subset doesn't include grouping vars", {
  by_cyl <- as.data.table(mtcars) %>% group_by(cyl)
  no_cyl <- by_cyl %>% `[`(c(1, 3))

  expect_no_groups(no_cyl)
  expect_is(no_cyl, "tbl_dt")
})

test_that("group_by works after arrange (#959)", {
  df  <- data.table(Log = c(1, 2, 1, 2, 1, 2), Time = c(10, 1, 3, 0, 15, 11))
  res <- df %>%
    arrange(Time) %>%
    group_by(Log) %>%
    mutate(Diff = Time - lag(Time))
  expect_true(all(is.na(res$Diff[c(1, 3)])))
  expect_equal(res$Diff[c(2, 4, 5, 6)], c(1, 7, 10, 5))
})

test_that("group_by keeps attributes", {
  d <- data.table(x = structure(1:10, foo = "bar"))
  gd <- group_by(d)
  expect_equal(attr(gd$x, "foo"), "bar")
})

# test_that("ungroup.rowwise_df gives a tbl_dt (#936)", {
#   res <- tbl_dt(mtcars) %>% rowwise %>% ungroup %>% class
#   expect_equal(res, c("tbl_dt", "tbl", "data.table"))
# })

# test_that(paste0("group_by handles encodings for native strings (#1507)"), {
#   with_non_utf8_encoding({
#     special <- get_native_lang_string()

#     df <- data.table(x = 1:3, Eng = 2:4)

#     for (names_converter in c(enc2native, enc2utf8)) {
#       for (dots_converter in c(enc2native, enc2utf8)) {
#         names(df) <- names_converter(c(special, "Eng"))
#         res <- group_by(df, !!! syms(dots_converter(special)))
#         expect_equal(names(res), names(df))
#         expect_groups(res, special)
#       }
#     }

#     for (names_converter in c(enc2native, enc2utf8)) {
#       names(df) <- names_converter(c(special, "Eng"))

#       res <- group_by(df, !!! special)
#       expect_equal(names(res), c(names(df), deparse(special)))
#       expect_equal(groups(res), list(as.name(enc2native(deparse(special)))))
#     }
#   })
# })

# test_that("group_by fails gracefully on raw columns (#1803)", {
#   df <- data.table(a = 1:3, b = as.raw(1:3))
#   expect_error(
#     group_by(df, a),
#     "Column `b` is of unsupported type raw",
#     fixed = TRUE
#   )
#   expect_error(
#     group_by(df, b),
#     "Column `b` is of unsupported type raw",
#     fixed = TRUE
#   )
# })

test_that("rowwise fails gracefully on raw columns (#1803)", {
  df <- data.table(a = 1:3, b = as.raw(1:3))
  expect_error(
    rowwise(df),
    "Column `b` is of unsupported type raw",
    fixed = TRUE
  )
})

test_that("group_by() names pronouns correctly (#2686)", {
  expect_named(group_by(tibble(x = 1), .data$x), "x")
  expect_named(group_by(tibble(x = 1), .data[["x"]]), "x")
})

test_that("group_by() does not affect input data (#3028)", {
  x <-
    data.table(old1 = c(1, 2, 3), old2 = c(4, 5, 6)) %>%
    group_by(old1)

  y <-
    x %>%
    select(new1 = old1, new2 = old2)

  expect_identical(groups(x), rlang::syms(quote(old1)))
})

