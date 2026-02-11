# Rename columns using their names

These are methods for the dplyr generics
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
and
[`dplyr::rename_with()`](https://dplyr.tidyverse.org/reference/rename.html).
They are both translated to
[`data.table::setnames()`](https://rdrr.io/pkg/data.table/man/setattr.html).

## Usage

``` r
# S3 method for class 'dtplyr_step'
rename(.data, ...)

# S3 method for class 'dtplyr_step'
rename_with(.data, .fn, .cols = everything(), ...)
```

## Arguments

- .data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md)

- ...:

  For [`rename()`](https://dplyr.tidyverse.org/reference/rename.html):
  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Use `new_name = old_name` to rename selected variables.

  For
  [`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html):
  additional arguments passed onto `.fn`.

- .fn:

  A function used to transform the selected `.cols`. Should return a
  character vector the same length as the input.

- .cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to rename; defaults to all columns.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
dt <- lazy_dt(data.frame(x = 1, y = 2, z = 3))
dt %>% rename(new_x = x, new_y = y)
#> Source: local data table [1 x 3]
#> Call:   setnames(copy(`_DT34`), c("x", "y"), c("new_x", "new_y"))
#> 
#>   new_x new_y     z
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% rename_with(toupper)
#> Source: local data table [1 x 3]
#> Call:   setnames(copy(`_DT34`), toupper)
#> 
#>       X     Y     Z
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
