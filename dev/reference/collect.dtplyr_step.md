# Force computation of a lazy data.table

- [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
  returns a tibble, grouped if needed.

- [`compute()`](https://dplyr.tidyverse.org/reference/compute.html)
  generates an intermediate assignment in the translation.

- [`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html)
  returns a data.table.

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  a data frame.

- [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  returns a tibble.

## Usage

``` r
# S3 method for class 'dtplyr_step'
collect(x, ...)

# S3 method for class 'dtplyr_step'
compute(x, name = unique_name(), ...)

# S3 method for class 'dtplyr_step'
as.data.table(x, keep.rownames = FALSE, ...)

# S3 method for class 'dtplyr_step'
as.data.frame(x, ...)

# S3 method for class 'dtplyr_step'
as_tibble(x, ..., .name_repair = "check_unique")
```

## Arguments

- x:

  A [lazy_dt](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md)

- ...:

  Arguments used by other methods.

- name:

  Name of intermediate data.table.

- keep.rownames:

  Ignored as dplyr never preserves rownames.

- .name_repair:

  Treatment of problematic column names

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(mtcars)

# Generate translation
avg_mpg <- dt %>%
  filter(am == 1) %>%
  group_by(cyl) %>%
  summarise(mpg = mean(mpg))

# Show translation and temporarily compute result
avg_mpg
#> Source: local data table [3 x 2]
#> Call:   `_DT3`[am == 1][, .(mpg = mean(mpg)), keyby = .(cyl)]
#> 
#>     cyl   mpg
#>   <dbl> <dbl>
#> 1     4  28.1
#> 2     6  20.6
#> 3     8  15.4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# compute and return tibble
avg_mpg_tb <- as_tibble(avg_mpg)
avg_mpg_tb
#> # A tibble: 3 × 2
#>     cyl   mpg
#>   <dbl> <dbl>
#> 1     4  28.1
#> 2     6  20.6
#> 3     8  15.4

# compute and return data.table
avg_mpg_dt <- data.table::as.data.table(avg_mpg)
avg_mpg_dt
#> Key: <cyl>
#>      cyl      mpg
#>    <num>    <num>
#> 1:     4 28.07500
#> 2:     6 20.56667
#> 3:     8 15.40000

# modify translation to use intermediate assignment
compute(avg_mpg)
#> Source: local data table [3 x 2]
#> Call:
#>   _DT4 <- `_DT3`[am == 1][, .(mpg = mean(mpg)), keyby = .(cyl)]
#>   `_DT4`
#> 
#>     cyl   mpg
#>   <dbl> <dbl>
#> 1     4  28.1
#> 2     6  20.6
#> 3     8  15.4
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
