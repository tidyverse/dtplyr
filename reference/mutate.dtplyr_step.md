# Create and modify columns

This is a method for the dplyr
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
generic. It is translated to the `j` argument of `[.data.table`, using
`:=` to modify "in place". If `.before` or `.after` is provided, the new
columns are relocated with a call to
[`data.table::setcolorder()`](https://rdrr.io/pkg/data.table/man/setcolorder.html).

## Usage

``` r
# S3 method for class 'dtplyr_step'
mutate(
  .data,
  ...,
  .by = NULL,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
)
```

## Arguments

- .data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs. The name gives the name of the column in the output.

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

  - `NULL`, to remove the column.

  - A data frame or tibble, to create multiple columns in the output.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .keep:

  Control which columns from `.data` are retained in the output.
  Grouping columns and columns created by `...` are always kept.

  - `"all"` retains all columns from `.data`. This is the default.

  - `"used"` retains only the columns used in `...` to create new
    columns. This is useful for checking your work, as it displays
    inputs and outputs side-by-side.

  - `"unused"` retains only the columns *not* used in `...` to create
    new columns. This is useful if you generate new columns, but no
    longer need the columns used to generate them.

  - `"none"` doesn't retain any extra columns from `.data`. Only the
    grouping variables and columns created by `...` are kept.

  Note: With dtplyr `.keep` will only work with column names passed as
  symbols, and won't work with other workflows (e.g.
  `eval(parse(text = "x + 1"))`)

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, control where new columns should appear (the default is to
  add to the right hand side). See
  [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
  for more details.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(data.frame(x = 1:5, y = 5:1))
dt %>%
  mutate(a = (x + y) / 2, b = sqrt(x^2 + y^2))
#> Source: local data table [5 x 4]
#> Call:   copy(`_DT23`)[, `:=`(a = (x + y)/2, b = sqrt(x^2 + y^2))]
#> 
#>       x     y     a     b
#>   <int> <int> <dbl> <dbl>
#> 1     1     5     3  5.10
#> 2     2     4     3  4.47
#> 3     3     3     3  4.24
#> 4     4     2     3  4.47
#> 5     5     1     3  5.10
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# It uses a more sophisticated translation when newly created variables
# are used in the same expression
dt %>%
  mutate(x1 = x + 1, x2 = x1 + 1)
#> Source: local data table [5 x 4]
#> Call:   copy(`_DT23`)[, `:=`(c("x1", "x2"), {
#>     x1 <- x + 1
#>     x2 <- x1 + 1
#>     .(x1, x2)
#> })]
#> 
#>       x     y    x1    x2
#>   <int> <int> <dbl> <dbl>
#> 1     1     5     2     3
#> 2     2     4     3     4
#> 3     3     3     4     5
#> 4     4     2     5     6
#> 5     5     1     6     7
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
