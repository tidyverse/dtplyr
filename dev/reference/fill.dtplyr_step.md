# Fill in missing values with previous or next value

This is a method for the tidyr
[`fill()`](https://tidyr.tidyverse.org/reference/fill.html) generic. It
is translated to
[`data.table::nafill()`](https://rdatatable.gitlab.io/data.table/reference/nafill.html).
Note that
[`data.table::nafill()`](https://rdatatable.gitlab.io/data.table/reference/nafill.html)
currently only works for integer and double columns.

## Usage

``` r
# S3 method for class 'dtplyr_step'
fill(data, ..., .direction = c("down", "up", "downup", "updown"))
```

## Arguments

- data:

  A data frame.

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to fill.

- .direction:

  Direction in which to fill missing values. Currently either "down"
  (the default), "up", "downup" (i.e. first down and then up) or
  "updown" (first up and then down).

## Examples

``` r
library(tidyr)

# Value (year) is recorded only when it changes
sales <- lazy_dt(tibble::tribble(
  ~quarter, ~year, ~sales,
  "Q1",    2000,    66013,
  "Q2",      NA,    69182,
  "Q3",      NA,    53175,
  "Q4",      NA,    21001,
  "Q1",    2001,    46036,
  "Q2",      NA,    58842,
  "Q3",      NA,    44568,
  "Q4",      NA,    50197,
  "Q1",    2002,    39113,
  "Q2",      NA,    41668,
  "Q3",      NA,    30144,
  "Q4",      NA,    52897,
  "Q1",    2004,    32129,
  "Q2",      NA,    67686,
  "Q3",      NA,    31768,
  "Q4",      NA,    49094
))

# `fill()` defaults to replacing missing data from top to bottom
sales %>% fill(year)
#> Source: local data table [16 x 3]
#> Call:   copy(`_DT10`)[, `:=`(year = nafill(year, "locf"))]
#> 
#>   quarter  year sales
#>   <chr>   <dbl> <dbl>
#> 1 Q1       2000 66013
#> 2 Q2       2000 69182
#> 3 Q3       2000 53175
#> 4 Q4       2000 21001
#> 5 Q1       2001 46036
#> 6 Q2       2001 58842
#> # ℹ 10 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Value (n_squirrels) is missing above and below within a group
squirrels <- lazy_dt(tibble::tribble(
  ~group,    ~name,     ~role,     ~n_squirrels,
  1,      "Sam",    "Observer",   NA,
  1,     "Mara", "Scorekeeper",    8,
  1,    "Jesse",    "Observer",   NA,
  1,      "Tom",    "Observer",   NA,
  2,     "Mike",    "Observer",   NA,
  2,  "Rachael",    "Observer",   NA,
  2,  "Sydekea", "Scorekeeper",   14,
  2, "Gabriela",    "Observer",   NA,
  3,  "Derrick",    "Observer",   NA,
  3,     "Kara", "Scorekeeper",    9,
  3,    "Emily",    "Observer",   NA,
  3, "Danielle",    "Observer",   NA
))

# The values are inconsistently missing by position within the group
# Use .direction = "downup" to fill missing values in both directions
squirrels %>%
  dplyr::group_by(group) %>%
  fill(n_squirrels, .direction = "downup") %>%
  dplyr::ungroup()
#> Source: local data table [12 x 4]
#> Call:   copy(`_DT12`)[, `:=`(n_squirrels = nafill(nafill(n_squirrels, 
#>     "locf"), "nocb")), by = .(group)]
#> 
#>   group name    role        n_squirrels
#>   <dbl> <chr>   <chr>             <dbl>
#> 1     1 Sam     Observer              8
#> 2     1 Mara    Scorekeeper           8
#> 3     1 Jesse   Observer              8
#> 4     1 Tom     Observer              8
#> 5     2 Mike    Observer             14
#> 6     2 Rachael Observer             14
#> # ℹ 6 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Using `.direction = "updown"` accomplishes the same goal in this example
```
