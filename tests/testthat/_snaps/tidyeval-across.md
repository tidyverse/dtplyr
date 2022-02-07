# across() gives informative errors

    Code
      capture_across(dt, across(a, 1))
    Error <rlang_error>
      `.fns` argument to dtplyr::across() must be a NULL, a function, formula, or list
    Code
      capture_across(dt, across(a, list(1)))
    Error <rlang_error>
      .fns argument to dtplyr::across() must contain a function or a formula
      x Problem with 1

# if_all() gives informative errors

    Code
      capture_if_all(dt, if_all(a, 1))
    Error <rlang_error>
      `.fns` argument to dtplyr::across() must be a NULL, a function, formula, or list
    Code
      capture_if_all(dt, if_all(a, list(1)))
    Error <rlang_error>
      .fns argument to dtplyr::across() must contain a function or a formula
      x Problem with 1

# if_all() cannot rename variables

    Code
      (expect_error(capture_if_all(dt, if_all(c(a = x, b = y)))))
    Output
      <error/rlang_error>
      Error in `ensure_named()`:
      ! Can't rename variables in this context.

