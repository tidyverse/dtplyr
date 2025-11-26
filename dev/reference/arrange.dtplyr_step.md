# Arrange rows by column values

This is a method for dplyr generic
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).
It is translated to an
[`order()`](https://rdatatable.gitlab.io/data.table/reference/setorder.html)
call in the `i` argument of `[.data.table`.

## Usage

``` r
# S3 method for class 'dtplyr_step'
arrange(.data, ..., .by_group = FALSE)
```

## Arguments

- .data:

  A
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .by_group:

  If `TRUE`, will sort first by grouping variable. Applies to grouped
  data frames only.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(mtcars)
dt %>% arrange(vs, cyl)
#> Source: local data table [32 x 11]
#> Call:   `_DT1`[order(vs, cyl)]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  26       4  120.    91  4.43  2.14  16.7     0     1     5     2
#> 2  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 3  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#> 4  19.7     6  145    175  3.62  2.77  15.5     0     1     5     6
#> 5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#> 6  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#> # ℹ 26 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% arrange(desc(vs), cyl)
#> Source: local data table [32 x 11]
#> Call:   `_DT1`[order(-vs, cyl)]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4 108      93  3.85  2.32  18.6     1     1     4     1
#> 2  24.4     4 147.     62  3.69  3.19  20       1     0     4     2
#> 3  22.8     4 141.     95  3.92  3.15  22.9     1     0     4     2
#> 4  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1
#> 5  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 6  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> # ℹ 26 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% arrange(across(mpg:disp))
#> Source: local data table [32 x 11]
#> Call:   `_DT1`[order(mpg, cyl, disp)]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  10.4     8   460   215  3     5.42  17.8     0     0     3     4
#> 2  10.4     8   472   205  2.93  5.25  18.0     0     0     3     4
#> 3  13.3     8   350   245  3.73  3.84  15.4     0     0     3     4
#> 4  14.3     8   360   245  3.21  3.57  15.8     0     0     3     4
#> 5  14.7     8   440   230  3.23  5.34  17.4     0     0     3     4
#> 6  15       8   301   335  3.54  3.57  14.6     0     1     5     8
#> # ℹ 26 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
