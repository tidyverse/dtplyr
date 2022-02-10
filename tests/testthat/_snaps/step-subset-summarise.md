# summarise(.groups=)

    Code
      eval_bare(expr(lazy_dt(data.frame(x = 1, y = 2), "DT") %>% group_by(x, y) %>%
        dplyr::summarise() %>% show_query()), env(global_env()))
    Message <rlang_message>
      `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
    Output
      unique(DT)

---

    `.groups` can't be "rowwise" in dtplyr
    i Possible values are NULL (default), "drop_last", "drop", and "keep"

