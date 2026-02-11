# Separate a character column into multiple columns with a regular expression or numeric locations

This is a method for the
[`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html)
generic. It is translated to
[`data.table::tstrsplit()`](https://rdrr.io/pkg/data.table/man/tstrsplit.html)
in the `j` argument of `[.data.table`.

## Usage

``` r
# S3 method for class 'dtplyr_step'
separate(
  data,
  col,
  into,
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  convert = FALSE,
  ...
)
```

## Arguments

- data:

  A [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md).

- col:

  Column name or position.

  This argument is passed by expression and supports quasiquotation (you
  can unquote column names or column positions).

- into:

  Names of new variables to create as character vector. Use `NA` to omit
  the variable in the output.

- sep:

  Separator between columns. The default value is a regular expression
  that matches any sequence of non-alphanumeric values.

- remove:

  If TRUE, remove the input column from the output data frame.

- convert:

  If TRUE, will run type.convert() with as.is = TRUE on new columns.
  This is useful if the component columns are integer, numeric or
  logical.

  NB: this will cause string "NA"s to be converted to NAs.

- ...:

  Arguments passed on to methods

## Examples

``` r
library(tidyr)
# If you want to split by any non-alphanumeric value (the default):
df <- lazy_dt(data.frame(x = c(NA, "x.y", "x.z", "y.z")), "DT")
df %>% separate(x, c("A", "B"))
#> Source: local data table [4 x 2]
#> Call:   copy(DT)[, `:=`(c("A", "B"), tstrsplit(x, split = "[^[:alnum:]]+"))][, 
#>     `:=`("x", NULL)]
#> 
#>   A     B    
#>   <chr> <chr>
#> 1 NA    NA   
#> 2 x     y    
#> 3 x     z    
#> 4 y     z    
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# If you just want the second variable:
df %>% separate(x, c(NA, "B"))
#> Source: local data table [4 x 1]
#> Call:   copy(DT)[, `:=`("B", tstrsplit(x, split = "[^[:alnum:]]+", keep = 2L))][, 
#>     `:=`("x", NULL)]
#> 
#>   B    
#>   <chr>
#> 1 NA   
#> 2 y    
#> 3 z    
#> 4 z    
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Use regular expressions to separate on multiple characters:
df <- lazy_dt(data.frame(x = c(NA, "x?y", "x.z", "y:z")), "DT")
df %>% separate(x, c("A","B"), sep = "([.?:])")
#> Source: local data table [4 x 2]
#> Call:   copy(DT)[, `:=`(c("A", "B"), tstrsplit(x, split = "([.?:])"))][, 
#>     `:=`("x", NULL)]
#> 
#>   A     B    
#>   <chr> <chr>
#> 1 NA    NA   
#> 2 x     y    
#> 3 x     z    
#> 4 y     z    
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# convert = TRUE detects column classes:
df <- lazy_dt(data.frame(x = c("x:1", "x:2", "y:4", "z", NA)), "DT")
df %>% separate(x, c("key","value"), ":") %>% str
#> List of 12
#>  $ parent         :List of 12
#>   ..$ parent         :List of 8
#>   .. ..$ parent       :Classes ‘data.table’ and 'data.frame':    5 obs. of  1 variable:
#>   .. .. ..$ x: chr [1:5] "x:1" "x:2" "y:4" "z" ...
#>   .. .. ..- attr(*, ".internal.selfref")=<externalptr> 
#>   .. ..$ vars         : chr "x"
#>   .. ..$ groups       : chr(0) 
#>   .. ..$ locals       : list()
#>   .. ..$ implicit_copy: logi FALSE
#>   .. ..$ needs_copy   : logi FALSE
#>   .. ..$ env          :<environment: 0x55c9d7b19688> 
#>   .. ..$ name         : symbol DT
#>   .. ..- attr(*, "class")= chr [1:2] "dtplyr_step_first" "dtplyr_step"
#>   ..$ vars           : chr [1:3] "x" "key" "value"
#>   ..$ groups         : chr(0) 
#>   ..$ locals         : list()
#>   ..$ implicit_copy  : logi TRUE
#>   ..$ needs_copy     : logi TRUE
#>   ..$ env            :<environment: 0x55c9d7b19688> 
#>   ..$ arrange        : NULL
#>   ..$ i              : NULL
#>   ..$ j              : language `:=`(c("key", "value"), tstrsplit(x, split = ":"))
#>   ..$ on             : chr(0) 
#>   ..$ allow_cartesian: NULL
#>   ..- attr(*, "class")= chr [1:2] "dtplyr_step_subset" "dtplyr_step"
#>  $ vars           : chr [1:2] "key" "value"
#>  $ groups         : chr(0) 
#>  $ locals         : list()
#>  $ implicit_copy  : logi TRUE
#>  $ needs_copy     : logi TRUE
#>  $ env            :<environment: 0x55c9d7b19688> 
#>  $ arrange        : NULL
#>  $ i              : NULL
#>  $ j              : language `:=`("x", NULL)
#>  $ on             : chr(0) 
#>  $ allow_cartesian: NULL
#>  - attr(*, "class")= chr [1:2] "dtplyr_step_subset" "dtplyr_step"
df %>% separate(x, c("key","value"), ":", convert = TRUE) %>% str
#> List of 12
#>  $ parent         :List of 12
#>   ..$ parent         :List of 8
#>   .. ..$ parent       :Classes ‘data.table’ and 'data.frame':    5 obs. of  1 variable:
#>   .. .. ..$ x: chr [1:5] "x:1" "x:2" "y:4" "z" ...
#>   .. .. ..- attr(*, ".internal.selfref")=<externalptr> 
#>   .. ..$ vars         : chr "x"
#>   .. ..$ groups       : chr(0) 
#>   .. ..$ locals       : list()
#>   .. ..$ implicit_copy: logi FALSE
#>   .. ..$ needs_copy   : logi FALSE
#>   .. ..$ env          :<environment: 0x55c9d7b19688> 
#>   .. ..$ name         : symbol DT
#>   .. ..- attr(*, "class")= chr [1:2] "dtplyr_step_first" "dtplyr_step"
#>   ..$ vars           : chr [1:3] "x" "key" "value"
#>   ..$ groups         : chr(0) 
#>   ..$ locals         : list()
#>   ..$ implicit_copy  : logi TRUE
#>   ..$ needs_copy     : logi TRUE
#>   ..$ env            :<environment: 0x55c9d7b19688> 
#>   ..$ arrange        : NULL
#>   ..$ i              : NULL
#>   ..$ j              : language `:=`(c("key", "value"), tstrsplit(x, split = ":", type.convert = TRUE))
#>   ..$ on             : chr(0) 
#>   ..$ allow_cartesian: NULL
#>   ..- attr(*, "class")= chr [1:2] "dtplyr_step_subset" "dtplyr_step"
#>  $ vars           : chr [1:2] "key" "value"
#>  $ groups         : chr(0) 
#>  $ locals         : list()
#>  $ implicit_copy  : logi TRUE
#>  $ needs_copy     : logi TRUE
#>  $ env            :<environment: 0x55c9d7b19688> 
#>  $ arrange        : NULL
#>  $ i              : NULL
#>  $ j              : language `:=`("x", NULL)
#>  $ on             : chr(0) 
#>  $ allow_cartesian: NULL
#>  - attr(*, "class")= chr [1:2] "dtplyr_step_subset" "dtplyr_step"
```
