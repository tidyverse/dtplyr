# can add groups if requested

    Code
      . <- dt %>% group_by(x) %>% group_by(y, add = TRUE)
    Condition
      Warning:
      The `add` argument of `group_by()` is deprecated as of dplyr 1.0.0.
      Please use the `.add` argument instead.

