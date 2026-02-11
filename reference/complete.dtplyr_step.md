# Complete a data frame with missing combinations of data

This is a method for the tidyr `complete()` generic. This is a wrapper
around `dtplyr` translations for `expand()`,
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
and `replace_na()` that's useful for completing missing combinations of
data.

## Usage

``` r
# S3 method for class 'dtplyr_step'
complete(data, ..., fill = list())
```

## Arguments

- data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

- ...:

  \<[`data-masking`](https://tidyr.tidyverse.org/reference/tidyr_data_masking.html)\>
  Specification of columns to expand or complete. Columns can be atomic
  vectors or lists.

  - To find all unique combinations of `x`, `y` and `z`, including those
    not present in the data, supply each variable as a separate
    argument: `expand(df, x, y, z)` or `complete(df, x, y, z)`.

  - To find only the combinations that occur in the data, use `nesting`:
    `expand(df, nesting(x, y, z))`.

  - You can combine the two forms. For example,
    `expand(df, nesting(school_id, student_id), date)` would produce a
    row for each present school-student combination for all possible
    dates.

  When used with factors,
  [`expand()`](https://tidyr.tidyverse.org/reference/expand.html) and
  [`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
  use the full set of levels, not just those that appear in the data. If
  you want to use only the values seen in the data, use
  `forcats::fct_drop()`.

  When used with continuous variables, you may need to fill in values
  that do not appear in the data: to do so use expressions like
  `year = 2010:2020` or `year = full_seq(year,1)`.

- fill:

  A named list that for each variable supplies a single value to use
  instead of `NA` for missing combinations.

## Examples

``` r
library(tidyr)
tbl <- tibble(x = 1:2, y = 1:2, z = 3:4)
dt <- lazy_dt(tbl)

dt %>%
  complete(x, y)
#> Source: local data table [4 x 3]
#> Call:   merge(`_DT5`[, CJ(x = x, y = y, unique = TRUE)], `_DT5`, all = TRUE, 
#>     by.x = c("x", "y"), by.y = c("x", "y"), allow.cartesian = TRUE)
#> 
#>       x     y     z
#>   <int> <int> <int>
#> 1     1     1     3
#> 2     1     2    NA
#> 3     2     1    NA
#> 4     2     2     4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

dt %>%
  complete(x, y, fill = list(z = 10L))
#> Source: local data table [4 x 3]
#> Call:   merge(`_DT5`[, CJ(x = x, y = y, unique = TRUE)], `_DT5`, all = TRUE, 
#>     by.x = c("x", "y"), by.y = c("x", "y"), allow.cartesian = TRUE)[, 
#>     `:=`(z = fcoalesce(z, 10L))]
#> 
#>       x     y     z
#>   <int> <int> <int>
#> 1     1     1     3
#> 2     1     2    10
#> 3     2     1    10
#> 4     2     2     4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
