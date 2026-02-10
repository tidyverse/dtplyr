# Drop rows containing missing values

This is a method for the tidyr
[`drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
generic. It is translated to
[`data.table::na.omit()`](https://rdrr.io/pkg/data.table/man/na.omit.data.table.html)

## Usage

``` r
# S3 method for class 'dtplyr_step'
drop_na(data, ...)
```

## Arguments

- data:

  A
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md).

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to inspect for missing values. If empty, all columns are used.

## Examples

``` r
library(dplyr)
library(tidyr)

dt <- lazy_dt(tibble(x = c(1, 2, NA), y = c("a", NA, "b")))
dt %>% drop_na()
#> Source: local data table [1 x 2]
#> Call:   na.omit(`_DT8`)
#> 
#>       x y    
#>   <dbl> <chr>
#> 1     1 a    
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% drop_na(x)
#> Source: local data table [2 x 2]
#> Call:   na.omit(`_DT8`, cols = "x")
#> 
#>       x y    
#>   <dbl> <chr>
#> 1     1 a    
#> 2     2 NA   
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

vars <- "y"
dt %>% drop_na(x, any_of(vars))
#> Source: local data table [1 x 2]
#> Call:   na.omit(`_DT8`, cols = c("x", "y"))
#> 
#>       x y    
#>   <dbl> <chr>
#> 1     1 a    
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
