# translates lag()/lead()

    The `order_by` argument of `lag()` is not supported by dtplyr

# errors when `where()` is used, #271/#368

    This tidyselect interface doesn't support predicates.

---

    This tidyselect interface doesn't support predicates.

# desc() checks the number of arguments

    Code
      capture_dot(df, desc(a, b))
    Condition
      Error in `dt_squash_call()`:
      ! `desc()` expects exactly one argument.

