# translates if_else()/ifelse()

    Code
      out_n <- capture_dot(df, ifelse(x < 0, n = 2, yes = 1))
    Condition
      Warning in `match.call()`:
      partial argument match of 'n' to 'no'
    Code
      out_f <- capture_dot(df, if_else(x < 0, f = 2, true = 1))
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

