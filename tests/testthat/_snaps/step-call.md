# but not with anything else

    Code
      dt %>% rename_with(1)
    Condition
      Error in `rename_with()`:
      ! `.fn` must be a function name or formula

# rename_with generates minimal spec

    Code
      dt %>% rename_with(toupper) %>% show_query()
    Output
      setnames(copy(DT), toupper)
    Code
      dt %>% rename_with(toupper, 1:3) %>% show_query()
    Output
      setnames(copy(DT), c("a", "b", "c"), toupper)

# can compute distinct computed variables

    Code
      dt %>% distinct(z = x + y) %>% show_query()
    Output
      unique(copy(dt)[, `:=`(z = x + y)][, `:=`(c("x", "y"), NULL)])

# errors are raised

    Code
      collect(drop_na(dt, "z"))
    Condition
      Error in `drop_na()`:
      ! Can't select columns that don't exist.
      x Column `z` doesn't exist.

