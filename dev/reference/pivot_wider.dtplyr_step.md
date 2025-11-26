# Pivot data from long to wide

This is a method for the tidyr
[`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
generic. It is translated to
[`data.table::dcast()`](https://rdatatable.gitlab.io/data.table/reference/dcast.data.table.html)

## Usage

``` r
# S3 method for class 'dtplyr_step'
pivot_wider(
  data,
  id_cols = NULL,
  names_from = name,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = NULL,
  ...
)
```

## Arguments

- data:

  A
  [`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md).

- id_cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A set of columns that uniquely identify each observation. Typically
  used when you have redundant variables, i.e. variables whose values
  are perfectly correlated with existing variables.

  Defaults to all columns in `data` except for the columns specified
  through `names_from` and `values_from`. If a tidyselect expression is
  supplied, it will be evaluated on `data` after removing the columns
  specified through `names_from` and `values_from`.

- names_from, values_from:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A pair of arguments describing which column (or columns) to get the
  name of the output column (`names_from`), and which column (or
  columns) to get the cell values from (`values_from`).

  If `values_from` contains multiple values, the value will be added to
  the front of the output column.

- names_prefix:

  String added to the start of every variable name. This is particularly
  useful if `names_from` is a numeric vector and you want to create
  syntactic variable names.

- names_sep:

  If `names_from` or `values_from` contains multiple variables, this
  will be used to join their values together into a single string to use
  as a column name.

- names_glue:

  Instead of `names_sep` and `names_prefix`, you can supply a glue
  specification that uses the `names_from` columns (and special
  `.value`) to create custom column names.

- names_sort:

  Should the column names be sorted? If `FALSE`, the default, column
  names are ordered by first appearance.

- names_repair:

  What happens if the output has invalid column names? The default,
  `"check_unique"` is to error if the columns are duplicated. Use
  `"minimal"` to allow duplicates in the output, or `"unique"` to
  de-duplicated by adding numeric suffixes. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for more options.

- values_fill:

  Optionally, a (scalar) value that specifies what each `value` should
  be filled in with when missing.

  This can be a named list if you want to apply different fill values to
  different value columns.

- values_fn:

  A function, the default is
  [`length()`](https://rdrr.io/r/base/length.html). Note this is
  different behavior than
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
  which returns a list column by default.

- ...:

  Additional arguments passed on to methods.

## Examples

``` r
library(tidyr)

fish_encounters_dt <- lazy_dt(fish_encounters)
fish_encounters_dt
#> Source: local data table [114 x 3]
#> Call:   `_DT29`
#> 
#>   fish  station  seen
#>   <fct> <fct>   <int>
#> 1 4842  Release     1
#> 2 4842  I80_1       1
#> 3 4842  Lisbon      1
#> 4 4842  Rstr        1
#> 5 4842  Base_TD     1
#> 6 4842  BCE         1
#> # ℹ 108 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
fish_encounters_dt %>%
  pivot_wider(names_from = station, values_from = seen)
#> Source: local data table [19 x 12]
#> Call:   dcast(`_DT29`, formula = fish ~ station, value.var = "seen")
#> 
#>   fish  Release I80_1 Lisbon  Rstr Base_TD   BCE   BCW  BCE2  BCW2
#>   <fct>   <int> <int>  <int> <int>   <int> <int> <int> <int> <int>
#> 1 4842        1     1      1     1       1     1     1     1     1
#> 2 4843        1     1      1     1       1     1     1     1     1
#> 3 4844        1     1      1     1       1     1     1     1     1
#> 4 4845        1     1      1     1       1    NA    NA    NA    NA
#> 5 4847        1     1      1    NA      NA    NA    NA    NA    NA
#> 6 4848        1     1      1     1      NA    NA    NA    NA    NA
#> # ℹ 13 more rows
#> # ℹ 2 more variables: MAE <int>, MAW <int>
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
# Fill in missing values
fish_encounters_dt %>%
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)
#> Source: local data table [19 x 12]
#> Call:   dcast(`_DT29`, formula = fish ~ station, value.var = "seen", 
#>     fill = 0)
#> 
#>   fish  Release I80_1 Lisbon  Rstr Base_TD   BCE   BCW  BCE2  BCW2
#>   <fct>   <int> <int>  <int> <int>   <int> <int> <int> <int> <int>
#> 1 4842        1     1      1     1       1     1     1     1     1
#> 2 4843        1     1      1     1       1     1     1     1     1
#> 3 4844        1     1      1     1       1     1     1     1     1
#> 4 4845        1     1      1     1       1     0     0     0     0
#> 5 4847        1     1      1     0       0     0     0     0     0
#> 6 4848        1     1      1     1       0     0     0     0     0
#> # ℹ 13 more rows
#> # ℹ 2 more variables: MAE <int>, MAW <int>
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Generate column names from multiple variables
us_rent_income_dt <- lazy_dt(us_rent_income)
us_rent_income_dt
#> Source: local data table [104 x 5]
#> Call:   `_DT30`
#> 
#>   GEOID NAME    variable estimate   moe
#>   <chr> <chr>   <chr>       <dbl> <dbl>
#> 1 01    Alabama income      24476   136
#> 2 01    Alabama rent          747     3
#> 3 02    Alaska  income      32940   508
#> 4 02    Alaska  rent         1200    13
#> 5 04    Arizona income      27517   148
#> 6 04    Arizona rent          972     4
#> # ℹ 98 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
us_rent_income_dt %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))
#> Source: local data table [52 x 6]
#> Call:   dcast(`_DT30`, formula = GEOID + NAME ~ variable, value.var = c("estimate", 
#> "moe"))
#> 
#>   GEOID NAME       estimate_income estimate_rent moe_income moe_rent
#>   <chr> <chr>                <dbl>         <dbl>      <dbl>    <dbl>
#> 1 01    Alabama              24476           747        136        3
#> 2 02    Alaska               32940          1200        508       13
#> 3 04    Arizona              27517           972        148        4
#> 4 05    Arkansas             23789           709        165        5
#> 5 06    California           29454          1358        109        3
#> 6 08    Colorado             32401          1125        109        5
#> # ℹ 46 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# When there are multiple `names_from` or `values_from`, you can use
# use `names_sep` or `names_glue` to control the output variable names
us_rent_income_dt %>%
  pivot_wider(
    names_from = variable,
    names_sep = ".",
    values_from = c(estimate, moe)
  )
#> Source: local data table [52 x 6]
#> Call:   dcast(`_DT30`, formula = GEOID + NAME ~ variable, value.var = c("estimate", 
#> "moe"), sep = ".")
#> 
#>   GEOID NAME       estimate.income estimate.rent moe.income moe.rent
#>   <chr> <chr>                <dbl>         <dbl>      <dbl>    <dbl>
#> 1 01    Alabama              24476           747        136        3
#> 2 02    Alaska               32940          1200        508       13
#> 3 04    Arizona              27517           972        148        4
#> 4 05    Arkansas             23789           709        165        5
#> 5 06    California           29454          1358        109        3
#> 6 08    Colorado             32401          1125        109        5
#> # ℹ 46 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results

# Can perform aggregation with values_fn
warpbreaks_dt <- lazy_dt(as_tibble(warpbreaks[c("wool", "tension", "breaks")]))
warpbreaks_dt
#> Source: local data table [54 x 3]
#> Call:   `_DT31`
#> 
#>   wool  tension breaks
#>   <fct> <fct>    <dbl>
#> 1 A     L           26
#> 2 A     L           30
#> 3 A     L           54
#> 4 A     L           25
#> 5 A     L           70
#> 6 A     L           52
#> # ℹ 48 more rows
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
warpbreaks_dt %>%
  pivot_wider(
    names_from = wool,
    values_from = breaks,
    values_fn = mean
  )
#> Source: local data table [3 x 3]
#> Call:   dcast(`_DT31`, formula = tension ~ wool, value.var = "breaks", 
#>     fun.aggregate = function (x, ...) 
#>     UseMethod("mean"))
#> 
#>   tension     A     B
#>   <fct>   <dbl> <dbl>
#> 1 L        44.6  28.2
#> 2 M        24    28.8
#> 3 H        24.6  18.8
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```
