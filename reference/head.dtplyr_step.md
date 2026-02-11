# Subset first or last rows

These are methods for the base generics
[`head()`](https://rdrr.io/r/utils/head.html) and
[`tail()`](https://rdrr.io/r/utils/head.html). They are not translated.

## Usage

``` r
# S3 method for class 'dtplyr_step'
head(x, n = 6L, ...)

# S3 method for class 'dtplyr_step'
tail(x, n = 6L, ...)
```

## Arguments

- x:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md)

- n:

  Number of rows to select. Can use a negative number to instead drop
  rows from the other end.

- ...:

  Passed on to
  [`head()`](https://rdrr.io/r/utils/head.html)/[`tail()`](https://rdrr.io/r/utils/head.html).

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
dt <- lazy_dt(data.frame(x = 1:10))

# first three rows
head(dt, 3)
#> Source: local data table [3 x 1]
#> Call:   head(`_DT17`, n = 3)
#> 
#>       x
#>   <int>
#> 1     1
#> 2     2
#> 3     3
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
# last three rows
tail(dt, 3)
#> Source: local data table [3 x 1]
#> Call:   tail(`_DT17`, n = 3)
#> 
#>       x
#>   <int>
#> 1     8
#> 2     9
#> 3    10
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# drop first three rows
tail(dt, -3)
#> Source: local data table [7 x 1]
#> Call:   tail(`_DT17`, n = -3)
#> 
#>       x
#>   <int>
#> 1     4
#> 2     5
#> 3     6
#> 4     7
#> 5     8
#> 6     9
#> # ℹ 1 more row
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
