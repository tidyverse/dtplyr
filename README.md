
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dtplyr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dtplyr)](https://cran.r-project.org/package=dtplyr)
[![Travis build
status](https://travis-ci.org/tidyverse/dtplyr.svg?branch=master)](https://travis-ci.org/tidyverse/dtplyr)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/dtplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/tidyverse/dtplyr?branch=master)
<!-- badges: end -->

## Overview

dtplyr provides a dplyr backend for
[data.table](https://github.com/Rdatatable/data.table/wiki). Compared to
the previous release, this version of dtplyr focusses only on lazy
evaluation triggered by use of `lazy_dt()`. This means that no
computation is performed until you explicitly request it with
`as.data.table()`, `as.data.frame()` or `as_tibble()`. This has a
considerable advantage over the previous version (which eagerly
evaluated each step) because it allows dtplyr to generate significantly
more performant translations. (See
[table.express](https://github.com/asardaes/table.express) and
[rqdatatable](https://github.com/WinVector/rqdatatable/) for related
work.)

See `vignette("translation")` for details of the current translations.

## Installation

You can install from CRAN with:

``` r
install.packages("dtplyr")
```

Or try the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("tidyverse/dtplyr")
```

## Usage

To use dtplyr, I recommend loading dplyr, dtplyr, and data.table:

``` r
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
```

Then use `lazy_dt()` to create a “lazy” data table that tracks the
operations performed to it.

``` r
mtcars2 <- lazy_dt(mtcars)
```

You can preview the transformation (including the generated data.table
code) by printing the result:

``` r
mtcars2 %>% 
  filter(wt < 5) %>% 
  mutate(l100k = 235.21 / mpg ) %>% # liters / 100 km
  group_by(cyl) %>% 
  summarise(l100k = mean(l100k))
#> Source: local data table [?? x 2]
#> Call:   `_DT1`[wt < 5, ][, `:=`(l100k = 235.21/mpg)][, .(l100k = mean(l100k)), 
#>     by = .(cyl)]
#> 
#>     cyl l100k
#>   <dbl> <dbl>
#> 1     6 12.0 
#> 2     4  9.05
#> 3     8 14.9 
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```

But generally you should reserve this only for debugging and use
`as.data.table()`, `as.data.frame()`, or `as_tibble()` to indicate that
you’re done with the transformation and want to access the results:

``` r
mtcars2 %>% 
  filter(wt < 5) %>% 
  mutate(l100k = 235.21 / mpg ) %>% # liters / 100 km
  group_by(cyl) %>% 
  summarise(l100k = mean(l100k)) %>% 
  as_tibble()
#> # A tibble: 3 x 2
#>     cyl l100k
#>   <dbl> <dbl>
#> 1     6 12.0 
#> 2     4  9.05
#> 3     8 14.9
```

## Why is dtplyr slower than data.table?

dtplyr will always be a bit slower than data.table, because it creates
copies of objects rather than mutating in place (that’s the dplyr
philosophy). Currently, dtplyr is quite a lot slower than bare
data.table because the methods aren’t quite smart enough. I hope
interested dplyr & data.table users from the community will help me to
improve the performance.

  - Some data.table expressions have no direct dplyr equivalent. For
    example, `X[Y, sum(foo*bar)]` selects the relevant variables
    *before* joining. The dplyr equivalent, `X %>% left_join(Y) %>%
    summarise(sum(foo * bar))`, carries along all variables in the join.

  - Each dplyr verb must do some computation to convert dplyr syntax to
    data.table syntax. This takes time proportional to the complexity of
    the input code, not the input *data*, so should be a negligible
    overhead for large datasets.

  - To match dplyr semantics, by default `mutate()` never modifies in
    place; so it must `copy()` the input first.
