# has useful display methods

    Code
      dt <- lazy_dt(mtcars, "DT")
      dt
    Output
      Source: local data table [32 x 11]
      Call:   DT
      
          mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
      2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
      3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
      4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
      5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2
      6  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1
      # i 26 more rows
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results
    Code
      dt %>% group_by(vs, am)
    Output
      Source: local data table [32 x 11]
      Groups: vs, am
      Call:   DT
      
          mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
      2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
      3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
      4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
      5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2
      6  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1
      # i 26 more rows
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results
    Code
      dt %>% mutate(y = 10) %>% compute("DT2")
    Output
      Source: local data table [32 x 12]
      Call:
        DT2 <- copy(DT)[, `:=`(y = 10)]
        DT2
      
          mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb     y
        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1  21       6   160   110  3.9   2.62  16.5     0     1     4     4    10
      2  21       6   160   110  3.9   2.88  17.0     0     1     4     4    10
      3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1    10
      4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1    10
      5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2    10
      6  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1    10
      # i 26 more rows
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results

# can print using n/max_extra_cols/max_footer_lines, #464, 

    Code
      dt <- letters %>% lapply(function(.x) tibble(!!.x := 1:10)) %>% bind_cols() %>%
        lazy_dt("DT")
      print(dt, n = 3)
    Output
      Source: local data table [10 x 26]
      Call:   DT
      
            a     b     c     d     e     f     g     h     i     j     k     l     m
        <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
      1     1     1     1     1     1     1     1     1     1     1     1     1     1
      2     2     2     2     2     2     2     2     2     2     2     2     2     2
      3     3     3     3     3     3     3     3     3     3     3     3     3     3
      # i 7 more rows
      # i 13 more variables: n <int>, o <int>, p <int>, q <int>, r <int>, s <int>,
      #   t <int>, u <int>, v <int>, w <int>, x <int>, y <int>, z <int>
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results
    Code
      print(dt, max_extra_cols = 3)
    Output
      Source: local data table [10 x 26]
      Call:   DT
      
            a     b     c     d     e     f     g     h     i     j     k     l     m
        <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
      1     1     1     1     1     1     1     1     1     1     1     1     1     1
      2     2     2     2     2     2     2     2     2     2     2     2     2     2
      3     3     3     3     3     3     3     3     3     3     3     3     3     3
      4     4     4     4     4     4     4     4     4     4     4     4     4     4
      5     5     5     5     5     5     5     5     5     5     5     5     5     5
      6     6     6     6     6     6     6     6     6     6     6     6     6     6
      # i 4 more rows
      # i 13 more variables: n <int>, o <int>, p <int>, ...
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results
    Code
      print(dt, max_footer_lines = 1)
    Output
      Source: local data table [10 x 26]
      Call:   DT
      
            a     b     c     d     e     f     g     h     i     j     k     l     m
        <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
      1     1     1     1     1     1     1     1     1     1     1     1     1     1
      2     2     2     2     2     2     2     2     2     2     2     2     2     2
      3     3     3     3     3     3     3     3     3     3     3     3     3     3
      4     4     4     4     4     4     4     4     4     4     4     4     4     4
      5     5     5     5     5     5     5     5     5     5     5     5     5     5
      6     6     6     6     6     6     6     6     6     6     6     6     6     6
      # i 4 more rows
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results

