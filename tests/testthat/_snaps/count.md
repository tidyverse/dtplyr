# can control name

    Code
      dt %>% count(name = 10) %>% collect()
    Condition
      Error in `tally()`:
      ! `name` must be a string

