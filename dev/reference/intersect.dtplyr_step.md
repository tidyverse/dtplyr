# Set operations

These are methods for the dplyr generics
[`generics::intersect()`](https://generics.r-lib.org/reference/setops.html),
[`generics::union()`](https://generics.r-lib.org/reference/setops.html),
[`dplyr::union_all()`](https://dplyr.tidyverse.org/reference/setops.html),
and
[`generics::setdiff()`](https://generics.r-lib.org/reference/setops.html).
They are translated to
[`data.table::fintersect()`](https://rdatatable.gitlab.io/data.table/reference/setops.html),
[`data.table::funion()`](https://rdatatable.gitlab.io/data.table/reference/setops.html),
and
[`data.table::fsetdiff()`](https://rdatatable.gitlab.io/data.table/reference/setops.html).

## Usage

``` r
# S3 method for class 'dtplyr_step'
intersect(x, y, ...)

# S3 method for class 'dtplyr_step'
union(x, y, ...)

# S3 method for class 'dtplyr_step'
union_all(x, y, ...)

# S3 method for class 'dtplyr_step'
setdiff(x, y, ...)
```

## Arguments

- x, y:

  A pair of
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md)s.

- ...:

  Ignored

## Examples

``` r
dt1 <- lazy_dt(data.frame(x = 1:4))
dt2 <- lazy_dt(data.frame(x = c(2, 4, 6)))

intersect(dt1, dt2)
#> Source: local data table [2 x 1]
#> Call:   fintersect(`_DT18`, `_DT19`)
#> 
#>       x
#>   <int>
#> 1     2
#> 2     4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
union(dt1, dt2)
#> Source: local data table [5 x 1]
#> Call:   funion(`_DT18`, `_DT19`)
#> 
#>       x
#>   <dbl>
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     6
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
setdiff(dt1, dt2)
#> Source: local data table [2 x 1]
#> Call:   fsetdiff(`_DT18`, `_DT19`)
#> 
#>       x
#>   <int>
#> 1     1
#> 2     3
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
