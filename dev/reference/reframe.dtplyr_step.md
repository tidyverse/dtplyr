# Summarise each group to one row

This is a method for the dplyr
[`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html)
generic. It is translated to the `j` argument of `[.data.table`.

## Usage

``` r
# S3 method for class 'dtplyr_step'
reframe(.data, ..., .by = NULL)
```

## Arguments

- .data:

  A
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>

  Name-value pairs of functions. The name will be the name of the
  variable in the result. The value can be a vector of any length.

  Unnamed data frame values add multiple columns from a single
  expression.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(mtcars)

dt %>%
  reframe(qs = quantile(disp, c(0.25, 0.75)),
          prob = c(0.25, 0.75),
          .by = cyl)
#> Source: local data table [6 x 3]
#> Call:   `_DT32`[, .(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 
#>     0.75)), keyby = .(cyl)]
#> 
#>     cyl    qs  prob
#>   <dbl> <dbl> <dbl>
#> 1     4  78.8  0.25
#> 2     4 121.   0.75
#> 3     6 160    0.25
#> 4     6 196.   0.75
#> 5     8 302.   0.25
#> 6     8 390    0.75
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

dt %>%
  group_by(cyl) %>%
  reframe(qs = quantile(disp, c(0.25, 0.75)),
          prob = c(0.25, 0.75))
#> Source: local data table [6 x 3]
#> Call:   `_DT32`[, .(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 
#>     0.75)), keyby = .(cyl)]
#> 
#>     cyl    qs  prob
#>   <dbl> <dbl> <dbl>
#> 1     4  78.8  0.25
#> 2     4 121.   0.75
#> 3     6 160    0.25
#> 4     6 196.   0.75
#> 5     8 302.   0.25
#> 6     8 390    0.75
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
