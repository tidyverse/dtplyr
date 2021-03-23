# can pivot to multiple measure cols

    Code
      show_query(step)
    Output
      melt(DT, measure.vars = list(c("x1", "x2", "x3", "x4"), c("y1", 
      "y2", "y3", "y4")), variable.name = "set", value.name = c("x", 
      "y"), variable.factor = FALSE)[, `:=`(set = c("1", "1", "2", 
      "2", "3", "3", "4", "4"))]

# errors on unbalanced datasets

    Code
      pivot_longer(dt, everything(), names_to = c(".value", "id"), names_sep = "_")
    Error <rlang_error>
      `data.table::melt()` doesn't currently support melting of unbalanced datasets.

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

