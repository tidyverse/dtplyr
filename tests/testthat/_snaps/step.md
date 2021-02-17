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
      # ... with 26 more rows
      
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
      # ... with 26 more rows
      
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
      # ... with 26 more rows
      
      # Use as.data.table()/as.data.frame()/as_tibble() to access results

