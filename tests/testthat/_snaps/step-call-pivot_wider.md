# names_glue affects output names & auto-converts data.table to lazy_dt

    Code
      show_query(step)
    Output
      setnames(dcast(`_DT5`, formula = "... ~ x + y", value.var = c("a", 
      "b"))[, .(a_X_1, a_Y_2, b_X_1, b_Y_2)], old = c("a_X_1", "a_Y_2", 
      "b_X_1", "b_Y_2"), new = c("X1_a", "Y2_a", "X1_b", "Y2_b"))

# can sort column names

    Code
      show_query(step)
    Output
      dcast(DT, formula = "... ~ chr", value.var = "int")[, .(Mon, 
          Tue, Wed)]

