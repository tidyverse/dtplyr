# Subset rows using their positions

These are methods for the dplyr
[`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html),
[`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html),
[`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html),
[`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
[`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html) and
[`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
generics. They are translated to the `i` argument of `[.data.table`.

Unlike dplyr,
[`slice()`](https://dplyr.tidyverse.org/reference/slice.html) (and
[`slice()`](https://dplyr.tidyverse.org/reference/slice.html) alone)
returns the same number of rows per group, regardless of whether or not
the indices appear in each group.

## Usage

``` r
# S3 method for class 'dtplyr_step'
slice(.data, ..., .by = NULL)

# S3 method for class 'dtplyr_step'
slice_head(.data, ..., n, prop, by = NULL)

# S3 method for class 'dtplyr_step'
slice_tail(.data, ..., n, prop, by = NULL)

# S3 method for class 'dtplyr_step'
slice_min(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE)

# S3 method for class 'dtplyr_step'
slice_max(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE)
```

## Arguments

- .data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

- ...:

  For [`slice()`](https://dplyr.tidyverse.org/reference/slice.html):
  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Integer row values.

  Provide either positive values to keep, or negative values to drop.
  The values provided must be either all positive or all negative.
  Indices beyond the number of rows in the input are silently ignored.

  For `slice_*()`, these arguments are passed on to methods.

- .by, by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- n, prop:

  Provide either `n`, the number of rows, or `prop`, the proportion of
  rows to select. If neither are supplied, `n = 1` will be used. If `n`
  is greater than the number of rows in the group (or `prop > 1`), the
  result will be silently truncated to the group size. `prop` will be
  rounded towards zero to generate an integer number of rows.

  A negative value of `n` or `prop` will be subtracted from the group
  size. For example, `n = -2` with a group of 5 rows will select 5 - 2 =
  3 rows; `prop = -0.25` with 8 rows will select 8 \* (1 - 0.25) = 6
  rows.

- order_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variable or function of variables to order by. To order by multiple
  variables, wrap them in a data frame or tibble.

- with_ties:

  Should ties be kept together? The default, `TRUE`, may return more
  rows than you request. Use `FALSE` to ignore ties, and return the
  first `n` rows.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(mtcars)
dt %>% slice(1, 5, 10)
#> Source: local data table [3 x 11]
#> Call:   `_DT37`[{
#>     .rows <- c(1, 5, 10)
#>     .rows[between(.rows, -.N, .N)]
#> }]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 2  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#> 3  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% slice(-(1:4))
#> Source: local data table [28 x 11]
#> Call:   `_DT37`[{
#>     .rows <- -(1:4)
#>     .rows[between(.rows, -.N, .N)]
#> }]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#> 2  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#> 3  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#> 4  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#> 5  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 6  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# First and last rows based on existing order
dt %>% slice_head(n = 5)
#> Source: local data table [5 x 11]
#> Call:   `_DT37`[rlang::seq2(1L, min(5L, .N))]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
#> 5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% slice_tail(n = 5)
#> Source: local data table [5 x 11]
#> Call:   `_DT37`[rlang::seq2(.N - min(5L, .N) + 1L, .N)]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  30.4     4  95.1   113  3.77  1.51  16.9     1     1     5     2
#> 2  15.8     8 351     264  4.22  3.17  14.5     0     1     5     4
#> 3  19.7     6 145     175  3.62  2.77  15.5     0     1     5     6
#> 4  15       8 301     335  3.54  3.57  14.6     0     1     5     8
#> 5  21.4     4 121     109  4.11  2.78  18.6     1     1     4     2
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Rows with minimum and maximum values of a variable
dt %>% slice_min(mpg, n = 5)
#> Source: local data table [5 x 11]
#> Call:   setorder(`_DT37`[rank(mpg, ties.method = "min", na.last = "keep") <= 
#>     min(5L, .N)], mpg, na.last = TRUE)
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  10.4     8   472   205  2.93  5.25  18.0     0     0     3     4
#> 2  10.4     8   460   215  3     5.42  17.8     0     0     3     4
#> 3  13.3     8   350   245  3.73  3.84  15.4     0     0     3     4
#> 4  14.3     8   360   245  3.21  3.57  15.8     0     0     3     4
#> 5  14.7     8   440   230  3.23  5.34  17.4     0     0     3     4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% slice_max(mpg, n = 5)
#> Source: local data table [5 x 11]
#> Call:   setorder(`_DT37`[rank(desc(mpg), ties.method = "min", na.last = "keep") <= 
#>     min(5L, .N)], -mpg, na.last = TRUE)
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> 2  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1
#> 3  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 4  30.4     4  95.1   113  3.77  1.51  16.9     1     1     5     2
#> 5  27.3     4  79      66  4.08  1.94  18.9     1     1     4     1
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# slice_min() and slice_max() may return more rows than requested
# in the presence of ties. Use with_ties = FALSE to suppress
dt %>% slice_min(cyl, n = 1)
#> Source: local data table [11 x 11]
#> Call:   setorder(`_DT37`[rank(cyl, ties.method = "min", na.last = "keep") <= 
#>     min(1L, .N)], cyl, na.last = TRUE)
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4 108      93  3.85  2.32  18.6     1     1     4     1
#> 2  24.4     4 147.     62  3.69  3.19  20       1     0     4     2
#> 3  22.8     4 141.     95  3.92  3.15  22.9     1     0     4     2
#> 4  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1
#> 5  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 6  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> # ℹ 5 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% slice_min(cyl, n = 1, with_ties = FALSE)
#> Source: local data table [1 x 11]
#> Call:   setorder(`_DT37`[rank(cyl, ties.method = "first", na.last = "keep") <= 
#>     min(1L, .N)], cyl, na.last = TRUE)
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# slice_sample() allows you to random select with or without replacement
dt %>% slice_sample(n = 5)
#> Source: local data table [5 x 11]
#> Call:   `_DT37`[sample.int(.N, min(min(5L, .N), .N))]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  16.4     8 276.    180  3.07  4.07  17.4     0     0     3     3
#> 2  21.4     4 121     109  4.11  2.78  18.6     1     1     4     2
#> 3  15.2     8 304     150  3.15  3.44  17.3     0     0     3     2
#> 4  17.3     8 276.    180  3.07  3.73  17.6     0     0     3     3
#> 5  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
dt %>% slice_sample(n = 5, replace = TRUE)
#> Source: local data table [5 x 11]
#> Call:   `_DT37`[sample.int(.N, min(5L, .N), replace = TRUE)]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> 2  21.4     6 258     110  3.08  3.22  19.4     1     0     3     1
#> 3  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 4  19.7     6 145     175  3.62  2.77  15.5     0     1     5     6
#> 5  15       8 301     335  3.54  3.57  14.6     0     1     5     8
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# you can optionally weight by a variable - this code weights by the
# physical weight of the cars, so heavy cars are more likely to get
# selected
dt %>% slice_sample(weight_by = wt, n = 5)
#> Source: local data table [5 x 11]
#> Call:   `_DT37`[sample.int(.N, min(min(5L, .N), .N), prob = wt)]
#> 
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4 141.     95  3.92  3.15  22.9     1     0     4     2
#> 2  22.8     4 108      93  3.85  2.32  18.6     1     1     4     1
#> 3  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 4  10.4     8 460     215  3     5.42  17.8     0     0     3     4
#> 5  15.5     8 318     150  2.76  3.52  16.9     0     0     3     2
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
