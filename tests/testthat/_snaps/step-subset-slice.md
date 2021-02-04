# check_slice_catches common errors

    Code
      check_slice_size(n = 1, prop = 1)
    Error <rlang_error>
      Must supply exactly one of `n` and `prop` arguments.
    Code
      check_slice_size(n = "a")
    Error <rlang_error>
      `n` must be a single number.
    Code
      check_slice_size(prop = "a")
    Error <rlang_error>
      `prop` must be a single number
    Code
      check_slice_size(n = -1)
    Error <rlang_error>
      `n` must be a non-missing positive number.
    Code
      check_slice_size(prop = -1)
    Error <rlang_error>
      `prop` must be a non-missing positive number.

