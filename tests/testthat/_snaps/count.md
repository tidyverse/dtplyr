# can control name

    Code
      dt %>% count(name = 10) %>% collect()
    Error <rlang_error>
      `name` must be a string

