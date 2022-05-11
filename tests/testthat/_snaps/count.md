# can control name

    Code
      dt %>% count(name = 10) %>% collect()
    Condition
      Error in `tally_count()`:
      ! `name` must be a string

