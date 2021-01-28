# can control name

    Code
      dt %>% count(name = 10) %>% collect()
    Error <rlang_error>
      `name` must be a string

# empty select returns no columns

    Code
      out <- lz %>% group_by(x) %>% select()
    Message <message>
      Adding missing grouping variables: `x`

