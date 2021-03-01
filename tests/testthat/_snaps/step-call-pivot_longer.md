# can pivot all cols to long

    Code
      show_query(step)
    Output
      melt(DT, measure.vars = c("x", "y"), variable.name = "name", 
          variable.factor = FALSE)

# preserves original keys

    Code
      show_query(step)
    Output
      melt(DT, measure.vars = c("y", "z"), variable.name = "name", 
          variable.factor = FALSE)

# can handle missing combinations

    Code
      show_query(step)
    Output
      melt(DT, measure.vars = list(c("x_1", "x_2"), "y_2"), variable.name = "n", 
          value.name = c("x", "y"), variable.factor = FALSE)

# can cast values cols

    Code
      show_query(step)
    Output
      melt(DT, measure.vars = c("x", "y"), variable.name = "name", 
          variable.factor = FALSE)[, `:=`(value = vec_cast(value, numeric(0)))]

# can coerce values cols

    Code
      show_query(step)
    Output
      melt(DT, measure.vars = c("x", "y"), variable.name = "name", 
          variable.factor = FALSE)[, `:=`(value = .Primitive("as.character")(value))]

