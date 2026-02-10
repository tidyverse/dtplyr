# Summarise each group to one row

This is a method for the dplyr
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
generic. It is translated to the `j` argument of `[.data.table`.

## Usage

``` r
# S3 method for class 'dtplyr_step'
summarise(.data, ..., .by = NULL, .groups = NULL)
```

## Arguments

- .data:

  A
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

  The value can be:

  - A vector of length 1, e.g. `min(x)`,
    [`n()`](https://dplyr.tidyverse.org/reference/context.html), or
    `sum(is.na(y))`.

  - A data frame with 1 row, to add multiple columns from a single
    expression.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .groups:

  **\[experimental\]** Grouping structure of the result.

  - `"drop_last"`: drops the last level of grouping. This was the only
    supported option before version 1.0.0.

  - `"drop"`: All levels of grouping are dropped.

  - `"keep"`: Same grouping structure as `.data`.

  - `"rowwise"`: Each row is its own group.

  When `.groups` is not specified, it is set to `"drop_last"` for a
  grouped data frame, and `"keep"` for a rowwise data frame. In
  addition, a message informs you of how the result will be grouped
  unless the result is ungrouped, the option `"dplyr.summarise.inform"`
  is set to `FALSE`, or when
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  is called from a function in a package.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(mtcars)

dt %>%
  group_by(cyl) %>%
  summarise(vs = mean(vs))
#> Source: local data table [3 x 2]
#> Call:   `_DT38`[, .(vs = mean(vs)), keyby = .(cyl)]
#> 
#>     cyl    vs
#>   <dbl> <dbl>
#> 1     4 0.909
#> 2     6 0.571
#> 3     8 0    
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

dt %>%
  group_by(cyl) %>%
  summarise(across(disp:wt, mean))
#> Source: local data table [3 x 5]
#> Call:   `_DT38`[, .(disp = mean(disp), hp = mean(hp), drat = mean(drat), 
#>     wt = mean(wt)), keyby = .(cyl)]
#> 
#>     cyl  disp    hp  drat    wt
#>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     4  105.  82.6  4.07  2.29
#> 2     6  183. 122.   3.59  3.12
#> 3     8  353. 209.   3.23  4.00
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
