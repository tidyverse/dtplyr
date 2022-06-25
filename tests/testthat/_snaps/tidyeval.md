# translates lag()/lead()

    The `order_by` argument of `lag()` is not supported by dtplyr

# errors when `where()` is used, #271/#368

    The use of `where()` is not supported by dtplyr.

---

    The use of `where()` is not supported by dtplyr.

# desc() checks the number of arguments

    Code
      capture_dot(df, desc(a, b))
    Condition
      Error in `dt_squash_call()`:
      ! `desc()` expects exactly one argument.

