# names_glue affects output names & auto-converts data.table to lazy_dt

    Code
      show_query(step)
    Output
      setnames(dcast(DT, formula = "..." ~ x + y, value.var = c("a", 
      "b"))[, .(a_X_1, a_Y_2, b_X_1, b_Y_2)], c("a_X_1", "a_Y_2", "b_X_1", 
      "b_Y_2"), c("X1_a", "Y2_a", "X1_b", "Y2_b"))

# can sort column names

    Code
      show_query(step)
    Output
      dcast(DT, formula = "..." ~ chr, value.var = "int")[, .(Mon, 
          Tue, Wed)]

# can sort column names with id

    Code
      show_query(step)
    Output
      setcolorder(dcast(DT, formula = id ~ chr, value.var = "int"), 
          c("id", "Mon", "Tue", "Wed"))

# can repair names if requested

    Code
      pivot_wider(df, names_from = lab, values_from = val)
    Error <vctrs_error_names_must_be_unique>
      Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.
    Code
      pivot_wider(df, names_from = lab, values_from = val, names_repair = "unique")
    Message <simpleMessage>
      New names:
      * x -> x...1
      * x -> x...2
    Output
      Source: local data table [1 x 2]
      Call:   setnames(dcast(copy(DT), formula = x ~ lab, value.var = "val"), 
          1:2, c("x...1", "x...2"))
      
        x...1 x...2
        <dbl> <dbl>
      1     1     2
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results

