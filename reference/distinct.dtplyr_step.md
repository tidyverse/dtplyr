# Subset distinct/unique rows

This is a method for the dplyr
[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
generic. It is translated to
[`data.table::unique.data.table()`](https://rdrr.io/pkg/data.table/man/duplicated.html).

## Usage

``` r
# S3 method for class 'dtplyr_step'
distinct(.data, ..., .keep_all = FALSE)
```

## Arguments

- .data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md)

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Optional variables to use when determining uniqueness. If there are
  multiple rows for a given combination of inputs, only the first row
  will be preserved. If omitted, will use all variables in the data
  frame.

- .keep_all:

  If `TRUE`, keep all variables in `.data`. If a combination of `...` is
  not distinct, this keeps the first row of values.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
df <- lazy_dt(data.frame(
  x = sample(10, 100, replace = TRUE),
  y = sample(10, 100, replace = TRUE)
))

df %>% distinct(x)
#> Source: local data table [10 x 1]
#> Call:   unique(`_DT7`[, .(x)])
#> 
#>       x
#>   <int>
#> 1     6
#> 2     9
#> 3     5
#> 4     8
#> 5     2
#> 6     7
#> # ℹ 4 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
df %>% distinct(x, y)
#> Source: local data table [59 x 2]
#> Call:   unique(`_DT7`)
#> 
#>       x     y
#>   <int> <int>
#> 1     6     3
#> 2     9     3
#> 3     5     1
#> 4     5    10
#> 5     8     7
#> 6     2     5
#> # ℹ 53 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
df %>% distinct(x, .keep_all = TRUE)
#> Source: local data table [10 x 2]
#> Call:   unique(`_DT7`, by = "x")
#> 
#>       x     y
#>   <int> <int>
#> 1     6     3
#> 2     9     3
#> 3     5     1
#> 4     8     7
#> 5     2     5
#> 6     7     4
#> # ℹ 4 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
