# but not with anything else

    Code
      dt %>% rename_with(1)
    Error <rlang_error>
      `.fn` must be a function name or formula

# rename_with generates minimal spec

    Code
      dt %>% rename_with(toupper) %>% show_query()
    Output
      setnames(copy(DT), toupper)
    Code
      dt %>% rename_with(toupper, 1:3) %>% show_query()
    Output
      setnames(copy(DT), c("a", "b", "c"), toupper)

