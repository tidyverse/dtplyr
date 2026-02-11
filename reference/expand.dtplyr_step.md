# Expand data frame to include all possible combinations of values.

This is a method for the tidyr
[`expand()`](https://tidyr.tidyverse.org/reference/expand.html) generic.
It is translated to
[`data.table::CJ()`](https://rdrr.io/pkg/data.table/man/J.html).

## Usage

``` r
# S3 method for class 'dtplyr_step'
expand(data, ..., .name_repair = "check_unique")
```

## Arguments

- data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

- ...:

  Specification of columns to expand. Columns can be atomic vectors or
  lists.

  - To find all unique combinations of `x`, `y` and `z`, including those
    not present in the data, supply each variable as a separate
    argument: `expand(df, x, y, z)`.

  - To find only the combinations that occur in the data, use `nesting`:
    `expand(df, nesting(x, y, z))`.

  - You can combine the two forms. For example,
    `expand(df, nesting(school_id, student_id), date)` would produce a
    row for each present school-student combination for all possible
    dates.

  Unlike the data.frame method, this method does not use the full set of
  levels, just those that appear in the data.

  When used with continuous variables, you may need to fill in values
  that do not appear in the data: to do so use expressions like
  `year = 2010:2020` or `year = full_seq(year,1)`.

- .name_repair:

  One of `"check_unique"`, `"unique"`, `"universal"`, `"minimal"`,
  `"unique_quiet"`, or `"universal_quiet"`. See
  [`vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for the meaning of these options.

## Examples

``` r
library(tidyr)

fruits <- lazy_dt(tibble(
  type   = c("apple", "orange", "apple", "orange", "orange", "orange"),
  year   = c(2010, 2010, 2012, 2010, 2010, 2012),
  size  =  factor(
    c("XS", "S",  "M", "S", "S", "M"),
    levels = c("XS", "S", "M", "L")
  ),
  weights = rnorm(6, as.numeric(size) + 2)
))

# All possible combinations ---------------------------------------
# Note that only present levels of the factor variable `size` are retained.
fruits %>% expand(type)
#> Source: local data table [2 x 1]
#> Call:   `_DT9`[, CJ(type = type, unique = TRUE)]
#> 
#>   type  
#>   <chr> 
#> 1 apple 
#> 2 orange
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
fruits %>% expand(type, size)
#> Source: local data table [6 x 2]
#> Call:   `_DT9`[, CJ(type = type, size = size, unique = TRUE)]
#> 
#>   type   size 
#>   <chr>  <fct>
#> 1 apple  XS   
#> 2 apple  S    
#> 3 apple  M    
#> 4 orange XS   
#> 5 orange S    
#> 6 orange M    
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# This is different from the data frame behaviour:
fruits %>% dplyr::collect() %>% expand(type, size)
#> # A tibble: 8 × 2
#>   type   size 
#>   <chr>  <fct>
#> 1 apple  XS   
#> 2 apple  S    
#> 3 apple  M    
#> 4 apple  L    
#> 5 orange XS   
#> 6 orange S    
#> 7 orange M    
#> 8 orange L    

# Other uses -------------------------------------------------------
fruits %>% expand(type, size, 2010:2012)
#> Source: local data table [18 x 3]
#> Call:   `_DT9`[, CJ(type = type, size = size, V3 = 2010:2012, unique = TRUE)]
#> 
#>   type  size     V3
#>   <chr> <fct> <int>
#> 1 apple XS     2010
#> 2 apple XS     2011
#> 3 apple XS     2012
#> 4 apple S      2010
#> 5 apple S      2011
#> 6 apple S      2012
#> # ℹ 12 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Use `anti_join()` to determine which observations are missing
all <- fruits %>% expand(type, size, year)
all
#> Source: local data table [12 x 3]
#> Call:   `_DT9`[, CJ(type = type, size = size, year = year, unique = TRUE)]
#> 
#>   type  size   year
#>   <chr> <fct> <dbl>
#> 1 apple XS     2010
#> 2 apple XS     2012
#> 3 apple S      2010
#> 4 apple S      2012
#> 5 apple M      2010
#> 6 apple M      2012
#> # ℹ 6 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
all %>% dplyr::anti_join(fruits)
#> Joining, by = c("type", "size", "year")
#> Source: local data table [8 x 3]
#> Call:   `_DT9`[, CJ(type = type, size = size, year = year, unique = TRUE)][!`_DT9`, 
#>     on = .(type, size, year)]
#> 
#>   type   size   year
#>   <chr>  <fct> <dbl>
#> 1 apple  XS     2012
#> 2 apple  S      2010
#> 3 apple  S      2012
#> 4 apple  M      2010
#> 5 orange XS     2010
#> 6 orange XS     2012
#> # ℹ 2 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Use with `right_join()` to fill in missing rows
fruits %>% dplyr::right_join(all)
#> Joining, by = c("type", "year", "size")
#> Source: local data table [14 x 4]
#> Call:   `_DT9`[`_DT9`[, CJ(type = type, size = size, year = year, unique = TRUE)], 
#>     on = .(type, year, size), allow.cartesian = TRUE]
#> 
#>   type   year size  weights
#>   <chr> <dbl> <fct>   <dbl>
#> 1 apple  2010 XS       4.05
#> 2 apple  2012 XS      NA   
#> 3 apple  2010 S       NA   
#> 4 apple  2012 S       NA   
#> 5 apple  2010 M       NA   
#> 6 apple  2012 M        5.04
#> # ℹ 8 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
