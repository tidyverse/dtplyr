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

