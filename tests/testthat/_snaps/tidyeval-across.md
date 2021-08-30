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

