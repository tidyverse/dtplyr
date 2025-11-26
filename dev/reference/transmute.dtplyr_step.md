# Create new columns, dropping old

This is a method for the dplyr
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
generic. It is translated to the `j` argument of `[.data.table`.

## Usage

``` r
# S3 method for class 'dtplyr_step'
transmute(.data, ...)
```

## Arguments

- .data:

  A
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs. The name gives the name of the column in the output.

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

  - `NULL`, to remove the column.

  - A data frame or tibble, to create multiple columns in the output.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(dplyr::starwars)
dt %>% transmute(name, sh = paste0(species, "/", homeworld))
#> Source: local data table [87 x 2]
#> Call:   copy(`_DT40`)[, `:=`(sh = paste0(species, "/", homeworld))][, 
#>     `:=`(c("height", "mass", "hair_color", "skin_color", "eye_color", 
#>     "birth_year", "sex", "gender", "homeworld", "species", "films", 
#>     "vehicles", "starships"), NULL)]
#> 
#>   name           sh            
#>   <chr>          <chr>         
#> 1 Luke Skywalker Human/Tatooine
#> 2 C-3PO          Droid/Tatooine
#> 3 R2-D2          Droid/Naboo   
#> 4 Darth Vader    Human/Tatooine
#> 5 Leia Organa    Human/Alderaan
#> 6 Owen Lars      Human/Tatooine
#> # ℹ 81 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
