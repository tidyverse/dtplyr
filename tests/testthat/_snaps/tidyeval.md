# translates if_else()/ifelse()

    Code
      expect_equal(capture_dot(df, ifelse(x < 0, n = 2, yes = 1)), expr(fifelse(x < 0,
      1, 2)))
    Condition
      Warning in `match.call()`:
      partial argument match of 'n' to 'no'
    Code
      expect_equal(capture_dot(df, if_else(x < 0, f = 2, true = 1)), expr(fifelse(x <
        0, 1, 2)))
    Condition
      Warning in `match.call()`:
      partial argument match of 'f' to 'false'

# translates lag()/lead()

    The `order_by` argument of `lag()` is not supported by dtplyr

# desc() checks the number of arguments

    Code
      capture_dot(df, desc(a, b))
    Condition
      Error in `dt_squash_call()`:
      ! `desc()` expects exactly one argument.

