# Relocate variables using their names

This is a method for the dplyr
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
generic. It is translated to the `j` argument of `[.data.table`.

## Usage

``` r
# S3 method for class 'dtplyr_step'
relocate(.data, ..., .before = NULL, .after = NULL)
```

## Arguments

- .data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to move.

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Destination of columns selected by `...`. Supplying neither will move
  columns to the left-hand side; specifying both is an error.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(data.frame(x = 1, y = 2, z = 3))

dt %>% relocate(z)
#> Source: local data table [1 x 3]
#> Call:   setcolorder(copy(`_DT33`), c("z", "x", "y"))
#> 
#>       z     x     y
#>   <dbl> <dbl> <dbl>
#> 1     3     1     2
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% relocate(y, .before = x)
#> Source: local data table [1 x 3]
#> Call:   setcolorder(copy(`_DT33`), c("y", "x", "z"))
#> 
#>       y     x     z
#>   <dbl> <dbl> <dbl>
#> 1     2     1     3
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% relocate(y, .after = y)
#> Source: local data table [1 x 3]
#> Call:   `_DT33`
#> 
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
