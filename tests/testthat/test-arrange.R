context("arrange")

df2 <- data.frame(
  a = rep(c(NA, 1, 2, 3), each = 4),
  b = rep(c(0L, NA, 1L, 2L), 4),
  c = c(NA, NA, NA, NA, letters[10:21]),
  d = rep( c(T, NA, F, T), each = 4),
  id = 1:16,
  stringsAsFactors = FALSE
)

equal_df <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  isTRUE(all.equal(x, y))
}


test_that("arrange results agree with data same regardless of backend", {
  # Can't check db because types are not currently preserved
  tbls <- list(df2, tbl_dt(df2))

  compare_tbls(tbls, function(x) x %>% arrange(a, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(b, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(c, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(d, id), compare = equal_df)

  compare_tbls(tbls, function(x) x %>% arrange(desc(a), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(desc(b), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(desc(c), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(desc(d), id), compare = equal_df)
})
