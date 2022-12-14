# slice_*() checks for empty ...

    Code
      slice_head(dt, 5)
    Condition
      Error in `slice_head()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 5
      i Did you forget to name an argument?
    Code
      slice_tail(dt, 5)
    Condition
      Error in `slice_tail()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 5
      i Did you forget to name an argument?
    Code
      slice_min(dt, x, 5)
    Condition
      Error in `slice_min_max()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 5
      i Did you forget to name an argument?
    Code
      slice_max(dt, x, 5)
    Condition
      Error in `slice_min_max()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 5
      i Did you forget to name an argument?
    Code
      slice_sample(dt, 5)
    Condition
      Error in `slice_sample()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 5
      i Did you forget to name an argument?

---

    Code
      slice_min(dt)
    Condition
      Error in `slice_min()`:
      ! argument `order_by` is missing, with no default.
    Code
      slice_max(dt)
    Condition
      Error in `slice_max()`:
      ! argument `order_by` is missing, with no default.

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

