# Subset rows using column values

This is a method for the dplyr
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
generic. It is translated to the `i` argument of `[.data.table`

## Usage

``` r
# S3 method for class 'dtplyr_step'
filter(.data, ..., .by = NULL, .preserve = FALSE)
```

## Arguments

- .data:

  A
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expressions that return a logical value, and are defined in terms of
  the variables in `.data`. If multiple expressions are included, they
  are combined with the `&` operator. Only rows for which all conditions
  evaluate to `TRUE` are kept.

- .by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .preserve:

  Ignored

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(mtcars)
dt %>% filter(cyl == 4)
#> Source: local data table [11 x 11]
#> Call:   `_DT14`[cyl == 4]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4 108      93  3.85  2.32  18.6     1     1     4     1
#> 2  24.4     4 147.     62  3.69  3.19  20       1     0     4     2
#> 3  22.8     4 141.     95  3.92  3.15  22.9     1     0     4     2
#> 4  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1
#> 5  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 6  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> # ℹ 5 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% filter(vs, am)
#> Source: local data table [7 x 11]
#> Call:   `_DT14`[vs & am]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4 108      93  3.85  2.32  18.6     1     1     4     1
#> 2  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1
#> 3  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 4  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> 5  27.3     4  79      66  4.08  1.94  18.9     1     1     4     1
#> 6  30.4     4  95.1   113  3.77  1.51  16.9     1     1     5     2
#> # ℹ 1 more row
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

dt %>%
  group_by(cyl) %>%
  filter(mpg > mean(mpg))
#> Source: local data table [16 x 11]
#> Groups: cyl
#> Call:   `_DT14`[`_DT14`[, .I[mpg > mean(mpg)], by = .(cyl)]$V1]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6 160     110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6 160     110  3.9   2.88  17.0     0     1     4     4
#> 3  21.4     6 258     110  3.08  3.22  19.4     1     0     3     1
#> 4  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1
#> 5  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 6  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> # ℹ 10 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
