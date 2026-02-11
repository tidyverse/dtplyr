# Group and ungroup

These are methods for dplyr's
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
and
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
generics. Grouping is translated to the either `keyby` and `by` argument
of `[.data.table` depending on the value of the `arrange` argument.

## Usage

``` r
# S3 method for class 'dtplyr_step'
group_by(.data, ..., .add = FALSE, arrange = TRUE)

# S3 method for class 'dtplyr_step'
ungroup(x, ...)
```

## Arguments

- .data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md)

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  In
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  variables or computations to group by. Computations are always done on
  the ungrouped data frame. To perform computations on the grouped data,
  you need to use a separate
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) step
  before the
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  Computations are not allowed in
  [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html). In
  [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
  variables to remove from the grouping.

- .add, add:

  When `FALSE`, the default,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  will override existing groups. To add to the existing groups, use
  `.add = TRUE`.

  This argument was previously called `add`, but that prevented creating
  a new grouping variable called `add`, and conflicts with our naming
  conventions.

- arrange:

  If `TRUE`, will automatically arrange the output of subsequent grouped
  operations by group. If `FALSE`, output order will be left unchanged.
  In the generated data.table code this switches between using the
  `keyby` (`TRUE`) and `by` (`FALSE`) arguments.

- x:

  A [`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
dt <- lazy_dt(mtcars)

# group_by() is usually translated to `keyby` so that the groups
# are ordered in the output
dt %>%
 group_by(cyl) %>%
 summarise(mpg = mean(mpg))
#> Source: local data table [3 x 2]
#> Call:   `_DT15`[, .(mpg = mean(mpg)), keyby = .(cyl)]
#> 
#>     cyl   mpg
#>   <dbl> <dbl>
#> 1     4  26.7
#> 2     6  19.7
#> 3     8  15.1
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# use `arrange = FALSE` to instead use `by` so the original order
# or groups is preserved
dt %>%
 group_by(cyl, arrange = FALSE) %>%
 summarise(mpg = mean(mpg))
#> Source: local data table [3 x 2]
#> Call:   `_DT15`[, .(mpg = mean(mpg)), by = .(cyl)]
#> 
#>     cyl   mpg
#>   <dbl> <dbl>
#> 1     6  19.7
#> 2     4  26.7
#> 3     8  15.1
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
