# Subset columns using their names

This is a method for the dplyr
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
generic. It is translated to the `j` argument of `[.data.table`.

## Usage

``` r
# S3 method for class 'dtplyr_step'
select(.data, ...)
```

## Arguments

- .data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(data.frame(x1 = 1, x2 = 2, y1 = 3, y2 = 4))

dt %>% select(starts_with("x"))
#> Source: local data table [1 x 2]
#> Call:   `_DT36`[, .(x1, x2)]
#> 
#>      x1    x2
#>   <dbl> <dbl>
#> 1     1     2
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% select(ends_with("2"))
#> Source: local data table [1 x 2]
#> Call:   `_DT36`[, .(x2, y2)]
#> 
#>      x2    y2
#>   <dbl> <dbl>
#> 1     2     4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% select(z1 = x1, z2 = x2)
#> Source: local data table [1 x 2]
#> Call:   `_DT36`[, .(z1 = x1, z2 = x2)]
#> 
#>      z1    z2
#>   <dbl> <dbl>
#> 1     1     2
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
