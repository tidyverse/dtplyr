# slice_*() checks for empty ...

    Code
      slice_head(dt, 5)
    Condition
      Error in `slice_head()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_head(n = 5)`?
    Code
      slice_tail(dt, 5)
    Condition
      Error in `slice_tail()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_tail(n = 5)`?
    Code
      slice_min(dt, x, 5)
    Condition
      Error in `slice_min()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_min(n = 5)`?
    Code
      slice_max(dt, x, 5)
    Condition
      Error in `slice_max()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_max(n = 5)`?
    Code
      slice_sample(dt, 5)
    Condition
      Error in `slice_sample()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_sample(n = 5)`?

---

    Code
      slice_min(dt)
    Condition
      Error in `slice_min()`:
      ! `order_by` is absent but must be supplied.
    Code
      slice_max(dt)
    Condition
      Error in `slice_max()`:
      ! `order_by` is absent but must be supplied.

# check_slice_catches common errors

    Code
      slice_head(dt, n = 1, prop = 1)
    Condition
      Error in `slice_head()`:
      ! Must supply exactly one of `n` and `prop` arguments.
    Code
      slice_head(dt, n = "a")
    Condition
      Error in `slice_head()`:
      ! `n` must be a single number.
    Code
      slice_head(dt, prop = "a")
    Condition
      Error in `slice_head()`:
      ! `prop` must be a single number.
    Code
      slice_head(dt, n = NA)
    Condition
      Error in `slice_head()`:
      ! `n` must be a single number.
    Code
      slice_head(dt, prop = NA)
    Condition
      Error in `slice_head()`:
      ! `prop` must be a single number.

