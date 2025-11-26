# Count observations by group

This is a method for the dplyr
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
generic. It is translated using `.N` in the `j` argument, and supplying
groups to `keyby` as appropriate.

## Usage

``` r
# S3 method for class 'dtplyr_step'
count(x, ..., wt = NULL, sort = FALSE, name = NULL)
```

## Arguments

- x:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md)

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to group by.

- wt:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Frequency weights. Can be `NULL` or a variable:

  - If `NULL` (the default), counts the number of rows in each group.

  - If a variable, computes `sum(wt)` for each group.

- sort:

  If `TRUE`, will show the largest groups at the top.

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(dplyr::starwars)
dt %>% count(species)
#> Source: local data table [38 x 2]
#> Call:   `_DT6`[, .(n = .N), keyby = .(species)]
#> 
#>   species      n
#>   <chr>    <int>
#> 1 NA           4
#> 2 Aleena       1
#> 3 Besalisk     1
#> 4 Cerean       1
#> 5 Chagrian     1
#> 6 Clawdite     1
#> # ℹ 32 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% count(species, sort = TRUE)
#> Source: local data table [38 x 2]
#> Call:   setorder(`_DT6`[, .(n = .N), keyby = .(species)], -n, na.last = TRUE)
#> 
#>   species      n
#>   <chr>    <int>
#> 1 Human       35
#> 2 Droid        6
#> 3 NA           4
#> 4 Gungan       3
#> 5 Kaminoan     2
#> 6 Mirialan     2
#> # ℹ 32 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% count(species, wt = mass, sort = TRUE)
#> Source: local data table [38 x 2]
#> Call:   setorder(`_DT6`[, .(n = sum(mass, na.rm = TRUE)), keyby = .(species)], 
#>     -n, na.last = TRUE)
#> 
#>   species     n
#>   <chr>   <dbl>
#> 1 Human   1626.
#> 2 Hutt    1358 
#> 3 Droid    279 
#> 4 Wookiee  248 
#> 5 NA       243 
#> 6 Kaleesh  159 
#> # ℹ 32 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
