# empty select returns no columns

    Code
      out <- lz %>% group_by(x) %>% select()
    Message <rlang_message>
      Adding missing grouping variables: `x`

