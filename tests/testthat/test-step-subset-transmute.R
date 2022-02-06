test_that("simple calls generate expected translations", {
  dt <- lazy_dt(data.table(x = 1, y = 1, z = 1), "DT")

  expect_equal(
    dt %>% transmute(x) %>% show_query(),
    expr(DT[, .(x = x)])
  )
})

test_that("transmute generates compound expression if needed", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  expect_equal(
    dt %>% transmute(x2 = x * 2, x4 = x2 * 2) %>% show_query(),
    expr(DT[, {
      x2 <- x * 2
      x4 <- x2 * 2
      .(x2, x4)
    }])
  )
})

test_that("allows multiple assignment to the same variable", {
  dt <- lazy_dt(data.table(x = 1, y = 2), "DT")

  # when nested
  expect_equal(
    dt %>% transmute(x = x * 2, x = x * 2) %>% show_query(),
    expr(DT[, {
      x <- x * 2
      x <- x * 2
      .(x)
    }])
  )

  # when not nested
  expect_equal(
    dt %>% transmute(z = 2, y = 3) %>% show_query(),
    expr(DT[, .(z = 2, y = 3)])
  )
})


test_that("groups are respected", {
  dt <- lazy_dt(data.table(x = 1), "DT") %>% group_by(x) %>% transmute(y = 2)

  expect_equal(dt$vars, c("x", "y"))
  expect_equal(
    dt %>% show_query(),
    expr(DT[, .(y = 2), keyby = .(x)])
  )
})

test_that("grouping vars can be transmuted", {
  dt <- lazy_dt(data.table(x = 1), "DT") %>% group_by(x) %>% transmute(x = x + 1, y = 2)

  expect_equal(dt$vars, c("x", "y"))
  expect_equal(dt$groups, "x")
  expect_equal(
    dt %>% show_query(),
    expr(copy(DT)[, `:=`(x = x + 1)][, .(y = 2), keyby = .(x)])
  )

  skip("transmuting grouping vars with nesting is not supported")
  dt <- lazy_dt(data.table(x = 1), "DT") %>%
    group_by(x) %>%
    transmute(x = x + 1, y = x + 1, x = y + 1)

  expect_equal(dt$vars, c("x", "y"))
  expect_equal(
    dt %>% collect(),
    tibble(x = 4, y = 3) %>% group_by(x)
  )
})

test_that("empty transmute works", {
  dt <- lazy_dt(data.frame(x = 1), "DT")
  expect_equal(transmute(dt) %>% show_query(), expr(DT[, 0L]))
  expect_equal(transmute(dt)$vars, character())
  expect_equal(transmute(dt, !!!list()) %>% show_query(), expr(DT[, 0L]))

  dt_grouped <- lazy_dt(data.frame(x = 1), "DT") %>% group_by(x)
  expect_equal(transmute(dt_grouped)$vars, "x")
})

test_that("only transmuting groups works", {
  dt <- lazy_dt(data.frame(x = 1)) %>% group_by(x)
  expect_equal(transmute(dt, x) %>% collect(), dt %>% collect())
  expect_equal(transmute(dt, x)$vars, "x")
})

test_that("across() can access previously created variables", {
  dt <- lazy_dt(data.frame(x = 1), "DT")
  step <- transmute(dt, y = 2, across(y, sqrt))
  expect_equal(
    collect(step),
    tibble(y = sqrt(2))
  )
  expect_equal(
    show_query(step),
    expr(DT[, {
      y <- 2
      y <- sqrt(y)
      .(y)
    }])
  )
})

test_that("new columns take precedence over global variables", {
  dt <- lazy_dt(data.frame(x = 1), "DT")
  y <- 'global var'
  step <- transmute(dt, y = 2, z = y + 1)
  expect_equal(
    collect(step),
    tibble(y = 2, z = 3)
  )
  expect_equal(
    show_query(step),
    expr(DT[, {
      y <- 2
      z <- y + 1
      .(y, z)
    }])
  )
})

# var = NULL -------------------------------------------------------------

test_that("var = NULL when var is in original data", {
  dt <- lazy_dt(data.frame(x = 1), "DT")
  step <- dt %>% transmute(x = 2, z = x*2, x = NULL)
  expect_equal(
    collect(step),
    tibble(z = 4)
  )
  expect_equal(
    step$vars,
    "z"
  )
  expect_equal(
    show_query(step),
    expr(DT[, {
      x <- 2
      z <- x * 2
      .(x, z)
    }][, `:=`("x", NULL)])
  )
})

test_that("var = NULL when var is in final output", {
  dt <- lazy_dt(data.frame(x = 1), "DT")
  step <- transmute(dt, y = NULL, y = 3)
  expect_equal(
    collect(step),
    tibble(y = 3)
  )
  expect_equal(
    show_query(step),
    expr(DT[, {
      y <- NULL
      y <- 3
      .(y)
    }])
  )
})

test_that("temp var with nested arguments", {
  dt <- lazy_dt(data.frame(x = 1), "DT")
  step <- transmute(dt, y = 2, z = y*2, y = NULL)
  expect_equal(
    collect(step),
    tibble(z = 4)
  )
  expect_equal(
    step$vars,
    "z"
  )
  expect_equal(
    show_query(step),
    expr(DT[, {
        y <- 2
        z <- y * 2
        .(y, z)
    }][, `:=`("y", NULL)])
  )
})

test_that("temp var with no new vars added", {
  dt <- lazy_dt(data.frame(x = 1), "DT")
  step <- transmute(dt, y = 2, y = NULL)
  expect_equal(
    collect(step),
    tibble()
  )
  expect_equal(
    step$vars,
    character()
  )
  expect_equal(
    show_query(step),
    expr(DT[, {
      y <- 2
      .(y)
    }][, `:=`("y", NULL)])
  )
})

test_that("var = NULL works when data is grouped", {
  dt <- lazy_dt(data.frame(x = 1, g = 1), "DT") %>% group_by(g)

  # when var is in original data
  step <- dt %>% transmute(x = 2, z = x*2, x = NULL)
  expect_equal(
    collect(step),
    tibble(g = 1, z = 4) %>% group_by(g)
  )
  expect_equal(
    step$vars,
    c("g", "z")
  )
  expect_equal(
    show_query(step),
    expr(DT[, {
      x <- 2
      z <- x * 2
      .(x, z)
    }, keyby = .(g)][, `:=`("x", NULL)])
  )

  # when var is not in original data
  step <- transmute(dt, y = 2, z = y*2, y = NULL)
  expect_equal(
    collect(step),
    tibble(g = 1, z = 4) %>% group_by(g)
  )
  expect_equal(
    step$vars,
    c("g", "z")
  )
  expect_equal(
    show_query(step),
    expr(DT[, {
      y <- 2
      z <- y * 2
      .(y, z)
    }, keyby = .(g)][, `:=`("y", NULL)])
  )
})

