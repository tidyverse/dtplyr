# can pivot to multiple measure cols

    Code
      show_query(step)
    Output
      melt(DT, measure.vars = list(c("x1", "x2", "x3", "x4"), c("y1", 
      "y2", "y3", "y4")), variable.name = "set", value.name = c("x", 
      "y"), variable.factor = FALSE)

# informative errors on unsupported features

    Code
      dt %>% pivot_longer(names_ptypes = list())
    Error <rlang_error>
      `names_ptypes` is not supported by dtplyr
    Code
      dt %>% pivot_longer(names_transform = list())
    Error <rlang_error>
      `names_transform` is not supported by dtplyr
    Code
      dt %>% pivot_longer(values_ptypes = list())
    Error <rlang_error>
      `values_ptypes` is not supported by dtplyr
    Code
      dt %>% pivot_longer(values_transform = list())
    Error <rlang_error>
      `values_transform` is not supported by dtplyr

