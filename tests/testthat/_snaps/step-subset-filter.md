# errors for named input

    Code
      filter(dt, x = 1)
    Error <rlang_error>
      Problem with `filter()` input `..1`.
      x Input `..1` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

---

    Code
      filter(dt, y > 1, x = 1)
    Error <rlang_error>
      Problem with `filter()` input `..2`.
      x Input `..2` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

