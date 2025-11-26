# Nest

This is a method for the tidyr
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html)
generic. It is translated using the non-nested variables in the `by`
argument and `.SD` in the `j` argument.

## Usage

``` r
# S3 method for class 'dtplyr_step'
nest(.data, ..., .names_sep = NULL, .key = deprecated())
```

## Arguments

- .data:

  A data frame.

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to nest, specified using name-variable pairs of the form
  `new_col = c(col1, col2, col3)`. The right hand side can be any valid
  tidy select expression.

- .names_sep:

  If `NULL`, the default, the inner names will come from the former
  outer names. If a string, the new inner names will use the outer names
  with `names_sep` automatically stripped. This makes `names_sep`
  roughly symmetric between nesting and unnesting.

- .key:

  Not supported.

- data:

  A
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md).

## Examples

``` r
if (require("tidyr", quietly = TRUE)) {
  dt <- lazy_dt(tibble(x = c(1, 2, 1), y = c("a", "a", "b")))
  dt %>% nest(data = y)

  dt %>% dplyr::group_by(x) %>% nest()
}
#> Source: local data table [2 x 2]
#> Groups: x
#> Call:   `_DT24`[, .(data = .(.SD)), by = .(x)]
#> 
#>       x data        
#>   <dbl> <list>      
#> 1     1 <dt [2 × 1]>
#> 2     2 <dt [1 × 1]>
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
