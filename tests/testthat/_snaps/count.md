# can control name

    Code
      dt %>% count(name = 10) %>% collect()
    Condition
      Error in `check_name()`:
      ! `name` must be a string

