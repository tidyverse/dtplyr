# empty select returns no columns

    Code
      out <- lz %>% group_by(x) %>% select()
    Message
      Adding missing grouping variables: `x`

