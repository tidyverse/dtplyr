# performs cross join

    Code
      left_join(dt1, dt2, by = character())
    Output
      Source: local data table [4 x 3]
      Call:   setnames(setcolorder(copy(dt2)[, `:=`(.cross_join_col = 1)][copy(dt1)[, 
          `:=`(.cross_join_col = 1)], on = .(.cross_join_col), allow.cartesian = TRUE], 
          c(3L, 4L, 2L, 1L)), c("i.x", "x"), c("x.x", "x.y"))[, !".cross_join_col"]
      
          x.x y       x.y
        <int> <chr> <int>
      1     1 a         3
      2     1 a         4
      3     2 a         3
      4     2 a         4
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results

---

    Code
      right_join(dt1, dt2, by = character())
    Output
      Source: local data table [4 x 3]
      Call:   setnames(setcolorder(copy(dt2)[, `:=`(.cross_join_col = 1)][copy(dt1)[, 
          `:=`(.cross_join_col = 1)], on = .(.cross_join_col), allow.cartesian = TRUE], 
          c(3L, 4L, 2L, 1L)), c("i.x", "x"), c("x.x", "x.y"))[, !".cross_join_col"]
      
          x.x y       x.y
        <int> <chr> <int>
      1     1 a         3
      2     1 a         4
      3     2 a         3
      4     2 a         4
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results

---

    Code
      full_join(dt1, dt2, by = character())
    Output
      Source: local data table [4 x 3]
      Call:   setnames(setcolorder(copy(dt2)[, `:=`(.cross_join_col = 1)][copy(dt1)[, 
          `:=`(.cross_join_col = 1)], on = .(.cross_join_col), allow.cartesian = TRUE], 
          c(3L, 4L, 2L, 1L)), c("i.x", "x"), c("x.x", "x.y"))[, !".cross_join_col"]
      
          x.x y       x.y
        <int> <chr> <int>
      1     1 a         3
      2     1 a         4
      3     2 a         3
      4     2 a         4
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results

---

    Code
      inner_join(dt1, dt2, by = character())
    Output
      Source: local data table [4 x 3]
      Call:   setnames(setcolorder(copy(dt2)[, `:=`(.cross_join_col = 1)][copy(dt1)[, 
          `:=`(.cross_join_col = 1)], on = .(.cross_join_col), allow.cartesian = TRUE], 
          c(3L, 4L, 2L, 1L)), c("i.x", "x"), c("x.x", "x.y"))[, !".cross_join_col"]
      
          x.x y       x.y
        <int> <chr> <int>
      1     1 a         3
      2     1 a         4
      3     2 a         3
      4     2 a         4
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results

