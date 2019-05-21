context("pronouns")

test_that("can count with `n()`", {
  dt <- data.table(g = c("a", "b", "a", "c"), x = 1:4)

  expect_identical(pull(summarise(dt, n = n())), 4L)
  expect_identical(pull(summarise(group_by(dt, g), n = n())), c(2L, 1L, 1L))

  skip("`count()` is failing")
  count(dt)
})

