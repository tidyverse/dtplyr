# can pivot to multiple measure cols

    Code
      show_query(step)
    Output
      melt(DT, measure.vars = list(c("x1", "x2", "x3", "x4"), c("y1", 
      "y2", "y3", "y4")), variable.name = "set", value.name = c("x", 
      "y"), variable.factor = FALSE)

