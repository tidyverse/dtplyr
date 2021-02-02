# empty select returns no columns

    Code
      out <- lz %>% group_by(x) %>% select()
    Message <message>
      Adding missing grouping variables: `x`

