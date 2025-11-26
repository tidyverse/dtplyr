# Join data tables

These are methods for the dplyr generics
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
and
[`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html).
Left, right, inner, and anti join are translated to the `[.data.table`
equivalent, full joins to
[`data.table::merge.data.table()`](https://rdatatable.gitlab.io/data.table/reference/merge.html).
Left, right, and full joins are in some cases followed by calls to
[`data.table::setcolorder()`](https://rdatatable.gitlab.io/data.table/reference/setcolorder.html)
and
[`data.table::setnames()`](https://rdatatable.gitlab.io/data.table/reference/setattr.html)
to ensure that column order and names match dplyr conventions.
Semi-joins don't have a direct data.table equivalent.

## Usage

``` r
# S3 method for class 'dtplyr_step'
left_join(x, y, ..., by = NULL, copy = FALSE, suffix = c(".x", ".y"))
```

## Arguments

- x, y:

  A pair of
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md)s.

- ...:

  Other parameters passed onto methods.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html), or
  a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.html) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

band_dt <- lazy_dt(dplyr::band_members)
instrument_dt <- lazy_dt(dplyr::band_instruments)

band_dt %>% left_join(instrument_dt)
#> Joining, by = "name"
#> Source: local data table [3 x 3]
#> Call:   setcolorder(`_DT22`[`_DT21`, on = .(name), allow.cartesian = TRUE], 
#>     c(1L, 3L, 2L))
#> 
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 Mick  Stones  NA    
#> 2 John  Beatles guitar
#> 3 Paul  Beatles bass  
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
band_dt %>% right_join(instrument_dt)
#> Joining, by = "name"
#> Source: local data table [3 x 3]
#> Call:   `_DT21`[`_DT22`, on = .(name), allow.cartesian = TRUE]
#> 
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 John  Beatles guitar
#> 2 Paul  Beatles bass  
#> 3 Keith NA      guitar
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
band_dt %>% inner_join(instrument_dt)
#> Joining, by = "name"
#> Source: local data table [2 x 3]
#> Call:   `_DT21`[`_DT22`, on = .(name), nomatch = NULL, allow.cartesian = TRUE]
#> 
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 John  Beatles guitar
#> 2 Paul  Beatles bass  
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
band_dt %>% full_join(instrument_dt)
#> Joining, by = "name"
#> Source: local data table [4 x 3]
#> Call:   merge(`_DT21`, `_DT22`, all = TRUE, by.x = "name", by.y = "name", 
#>     allow.cartesian = TRUE)
#> 
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 John  Beatles guitar
#> 2 Keith NA      guitar
#> 3 Mick  Stones  NA    
#> 4 Paul  Beatles bass  
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

band_dt %>% semi_join(instrument_dt)
#> Joining, by = "name"
#> Source: local data table [2 x 2]
#> Call:   `_DT21`[unique(`_DT21`[`_DT22`, which = TRUE, nomatch = NULL, 
#>     on = .(name)])]
#> 
#>   name  band   
#>   <chr> <chr>  
#> 1 John  Beatles
#> 2 Paul  Beatles
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
band_dt %>% anti_join(instrument_dt)
#> Joining, by = "name"
#> Source: local data table [1 x 2]
#> Call:   `_DT21`[!`_DT22`, on = .(name)]
#> 
#>   name  band  
#>   <chr> <chr> 
#> 1 Mick  Stones
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
