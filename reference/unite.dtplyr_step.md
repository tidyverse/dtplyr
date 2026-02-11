# Unite multiple columns into one by pasting strings together.

This is a method for the tidyr
[`unite()`](https://tidyr.tidyverse.org/reference/unite.html) generic.

## Usage

``` r
# S3 method for class 'dtplyr_step'
unite(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE)
```

## Arguments

- data:

  A data frame.

- col:

  The name of the new column, as a string or symbol.

  This argument is passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote strings and symbols). The name is captured from the
  expression with
  [`rlang::ensym()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  (note that this kind of interface where symbols do not represent
  actual objects is now discouraged in the tidyverse; we support it here
  for backward compatibility).

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to unite

- sep:

  Separator to use between values.

- remove:

  If `TRUE`, remove input columns from output data frame.

- na.rm:

  If `TRUE`, missing values will be removed prior to uniting each value.

## Examples

``` r
library(tidyr)

df <- lazy_dt(expand_grid(x = c("a", NA), y = c("b", NA)))
df
#> Source: local data table [4 x 2]
#> Call:   `_DT41`
#> 
#>   x     y    
#>   <chr> <chr>
#> 1 a     b    
#> 2 a     NA   
#> 3 NA    b    
#> 4 NA    NA   
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

df %>% unite("z", x:y, remove = FALSE)
#> Source: local data table [4 x 3]
#> Call:   setcolorder(copy(`_DT41`)[, `:=`(z = paste(x, y, sep = "_"))], 
#>     c("z", "x", "y"))
#> 
#>   z     x     y    
#>   <chr> <chr> <chr>
#> 1 a_b   a     b    
#> 2 a_NA  a     NA   
#> 3 NA_b  NA    b    
#> 4 NA_NA NA    NA   
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Separate is almost the complement of unite
df %>%
  unite("xy", x:y) %>%
  separate(xy, c("x", "y"))
#> Source: local data table [4 x 2]
#> Call:   copy(`_DT41`)[, `:=`(xy = paste(x, y, sep = "_"))][, `:=`(c("x", 
#> "y"), NULL)][, `:=`(c("x", "y"), tstrsplit(xy, split = "[^[:alnum:]]+"))][, 
#>     `:=`("xy", NULL)]
#> 
#>   x     y    
#>   <chr> <chr>
#> 1 a     b    
#> 2 a     NA   
#> 3 NA    b    
#> 4 NA    NA   
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
# (but note `x` and `y` contain now "NA" not NA)
```
