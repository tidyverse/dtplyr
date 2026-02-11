# Package index

## Getting data in and out

- [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md) :
  Create a "lazy" data.table for use with dplyr verbs
- [`collect(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/collect.dtplyr_step.md)
  [`compute(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/collect.dtplyr_step.md)
  [`as.data.table(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/collect.dtplyr_step.md)
  [`as.data.frame(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/collect.dtplyr_step.md)
  [`as_tibble(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/collect.dtplyr_step.md)
  : Force computation of a lazy data.table

## Single table verbs

- [`arrange(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/arrange.dtplyr_step.md)
  : Arrange rows by column values
- [`count(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/count.dtplyr_step.md)
  : Count observations by group
- [`distinct(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/distinct.dtplyr_step.md)
  : Subset distinct/unique rows
- [`filter(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/filter.dtplyr_step.md)
  : Subset rows using column values
- [`group_by(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/group_by.dtplyr_step.md)
  [`ungroup(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/group_by.dtplyr_step.md)
  : Group and ungroup
- [`group_modify(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/group_modify.dtplyr_step.md)
  [`group_map(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/group_modify.dtplyr_step.md)
  : Apply a function to each group
- [`head(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/head.dtplyr_step.md)
  [`tail(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/head.dtplyr_step.md)
  : Subset first or last rows
- [`mutate(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/mutate.dtplyr_step.md)
  : Create and modify columns
- [`transmute(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/transmute.dtplyr_step.md)
  : Create new columns, dropping old
- [`relocate(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/relocate.dtplyr_step.md)
  : Relocate variables using their names
- [`rename(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/rename.dtplyr_step.md)
  [`rename_with(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/rename.dtplyr_step.md)
  : Rename columns using their names
- [`reframe(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/reframe.dtplyr_step.md)
  : Summarise each group to one row
- [`select(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/select.dtplyr_step.md)
  : Subset columns using their names
- [`slice(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/slice.dtplyr_step.md)
  [`slice_head(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/slice.dtplyr_step.md)
  [`slice_tail(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/slice.dtplyr_step.md)
  [`slice_min(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/slice.dtplyr_step.md)
  [`slice_max(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/slice.dtplyr_step.md)
  : Subset rows using their positions
- [`summarise(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/summarise.dtplyr_step.md)
  : Summarise each group to one row

## Two table verbs

- [`left_join(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/left_join.dtplyr_step.md)
  : Join data tables
- [`intersect(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/intersect.dtplyr_step.md)
  [`union(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/intersect.dtplyr_step.md)
  [`union_all(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/intersect.dtplyr_step.md)
  [`setdiff(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/intersect.dtplyr_step.md)
  : Set operations

## tidyr verbs

- [`complete(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/complete.dtplyr_step.md)
  : Complete a data frame with missing combinations of data
- [`drop_na(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/drop_na.dtplyr_step.md)
  : Drop rows containing missing values
- [`expand(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/expand.dtplyr_step.md)
  : Expand data frame to include all possible combinations of values.
- [`fill(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/fill.dtplyr_step.md)
  : Fill in missing values with previous or next value
- [`nest(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/nest.dtplyr_step.md)
  : Nest
- [`pivot_wider(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/pivot_wider.dtplyr_step.md)
  : Pivot data from long to wide
- [`pivot_longer(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/pivot_longer.dtplyr_step.md)
  : Pivot data from wide to long
- [`replace_na(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/replace_na.dtplyr_step.md)
  : Replace NAs with specified values
- [`separate(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/separate.dtplyr_step.md)
  : Separate a character column into multiple columns with a regular
  expression or numeric locations
- [`unite(`*`<dtplyr_step>`*`)`](https://dtplyr.tidyverse.org/reference/unite.dtplyr_step.md)
  : Unite multiple columns into one by pasting strings together.
