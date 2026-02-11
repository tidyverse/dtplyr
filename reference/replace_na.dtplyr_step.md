# Replace NAs with specified values

This is a method for the tidyr
[`replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html)
generic. It is translated to
[`data.table::fcoalesce()`](https://rdrr.io/pkg/data.table/man/coalesce.html).

Note that unlike
[`tidyr::replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html),
[`data.table::fcoalesce()`](https://rdrr.io/pkg/data.table/man/coalesce.html)
cannot replace `NULL` values in lists.

## Usage

``` r
# S3 method for class 'dtplyr_step'
replace_na(data, replace = list())
```

## Arguments

- data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

- replace:

  If `data` is a data frame, `replace` takes a named list of values,
  with one value for each column that has missing values to be replaced.
  Each value in `replace` will be cast to the type of the column in
  `data` that it being used as a replacement in.

  If `data` is a vector, `replace` takes a single value. This single
  value replaces all of the missing values in the vector. `replace` will
  be cast to the type of `data`.

## Examples

``` r
library(tidyr)

# Replace NAs in a data frame
dt <- lazy_dt(tibble(x = c(1, 2, NA), y = c("a", NA, "b")))
dt %>% replace_na(list(x = 0, y = "unknown"))
#> Source: local data table [3 x 2]
#> Call:   copy(`_DT35`)[, `:=`(x = fcoalesce(x, 0), y = fcoalesce(y, "unknown"))]
#> 
#>       x y      
#>   <dbl> <chr>  
#> 1     1 a      
#> 2     2 unknown
#> 3     0 b      
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Replace NAs using `dplyr::mutate()`
dt %>% dplyr::mutate(x = replace_na(x, 0))
#> Source: local data table [3 x 2]
#> Call:   copy(`_DT35`)[, `:=`(x = fcoalesce(x, 0))]
#> 
#>       x y    
#>   <dbl> <chr>
#> 1     1 a    
#> 2     2 NA   
#> 3     0 b    
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
