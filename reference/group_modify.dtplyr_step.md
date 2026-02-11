# Apply a function to each group

These are methods for the dplyr
[`dplyr::group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
and
[`dplyr::group_modify()`](https://dplyr.tidyverse.org/reference/group_map.html)
generics. They are both translated to `[.data.table`.

## Usage

``` r
# S3 method for class 'dtplyr_step'
group_modify(.data, .f, ..., keep = FALSE)

# S3 method for class 'dtplyr_step'
group_map(.data, .f, ..., keep = FALSE)
```

## Arguments

- .data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md)

- .f:

  The name of a two argument function. The first argument is passed
  `.SD`,the data.table representing the current group; the second
  argument is passed `.BY`, a list giving the current values of the
  grouping variables. The function should return a list or data.table.

- ...:

  Additional arguments passed to `.f`

- keep:

  Not supported for
  [lazy_dt](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

## Value

[`group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
applies `.f` to each group, returning a list.
[`group_modify()`](https://dplyr.tidyverse.org/reference/group_map.html)
replaces each group with the results of `.f`, returning a modified
[`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

## Examples

``` r
library(dplyr)

dt <- lazy_dt(mtcars)

dt %>%
  group_by(cyl) %>%
  group_modify(head, n = 2L)
#> Source: local data table [6 x 11]
#> Groups: cyl
#> Call:   `_DT16`[, head(.SD, .BY, n = ~2L), by = .(cyl)]
#> 
#>     cyl   mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     6  21    160    110  3.9   2.62  16.5     0     1     4     4
#> 2     6  21    160    110  3.9   2.88  17.0     0     1     4     4
#> 3     4  22.8  108     93  3.85  2.32  18.6     1     1     4     1
#> 4     4  24.4  147.    62  3.69  3.19  20       1     0     4     2
#> 5     8  18.7  360    175  3.15  3.44  17.0     0     0     3     2
#> 6     8  14.3  360    245  3.21  3.57  15.8     0     0     3     4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

dt %>%
  group_by(cyl) %>%
  group_map(head, n = 2L)
#> [[1]]
#>      mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:    21   160   110   3.9 2.620 16.46     0     1     4     4
#> 2:    21   160   110   3.9 2.875 17.02     0     1     4     4
#> 
#> [[2]]
#>      mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:  22.8 108.0    93  3.85  2.32 18.61     1     1     4     1
#> 2:  24.4 146.7    62  3.69  3.19 20.00     1     0     4     2
#> 
#> [[3]]
#>      mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:  18.7   360   175  3.15  3.44 17.02     0     0     3     2
#> 2:  14.3   360   245  3.21  3.57 15.84     0     0     3     4
#> 
```
