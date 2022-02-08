# check_slice_catches common errors

    Code
      check_slice_size(n = 1, prop = 1)
    Condition
      Error in `check_slice_size()`:
      ! Must supply exactly one of `n` and `prop` arguments.
    Code
      check_slice_size(n = "a")
    Condition
      Error in `check_slice_size()`:
      ! `n` must be a single number.
    Code
      check_slice_size(prop = "a")
    Condition
      Error in `check_slice_size()`:
      ! `prop` must be a single number.
    Code
      check_slice_size(n = NA)
    Condition
      Error in `check_slice_size()`:
      ! `n` must be a single number.
    Code
      check_slice_size(prop = NA)
    Condition
      Error in `check_slice_size()`:
      ! `prop` must be a single number.

