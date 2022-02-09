# across() does not support formulas with dots

    Code
      (expect_error(capture_across(dt, across(a:b, ~ log(.x, base = .y), base = 2))))
    Output
      <error/rlang_error>
      Error in `across_fun()`:
      ! `dtplyr` does not support `...` in `across()` and `if_all()`.
      i Use a lambda instead.
      i Or inline them via purrr-style lambdas.
    Code
      (expect_error(capture_across(dt, across(a:b, list(~ log(.x, base = .y)), base = 2)))
      )
    Output
      <error/rlang_error>
      Error in `FUN()`:
      ! `dtplyr` does not support `...` in `across()` and `if_all()`.
      i Use a lambda instead.
      i Or inline them via purrr-style lambdas.

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

