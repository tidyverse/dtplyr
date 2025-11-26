# Translation

## Introduction

This vignette shows the details of how dtplyr translates dplyr
expressions into the equivalent [data.table](http://r-datatable.com/)
code. If you see places where you think I could generate better
data.table code, please [let me
know](https://github.com/tidyverse/dtplyr/issues)!

This document assumes that you’re familiar with the basics of
data.table; if you’re not, I recommend starting at
`vignette("datatable-intro.html")`.

``` r
library(dtplyr)
library(data.table)
library(dplyr)
```

## The basics

To get started, I’ll create a simple lazy table with
[`lazy_dt()`](https://dtplyr.tidyverse.org/dev/reference/lazy_dt.md):

``` r
df <- data.frame(a = 1:5, b = 1:5, c = 1:5, d = 1:5)
dt <- lazy_dt(df)
```

The actual data doesn’t matter here since we’re just looking at the
translation.

When you print a lazy frame, it tells you that it’s a local data table
with four rows. It also prints the call that dtplyr will evaluate when
we execute the lazy table. In this case it’s very simple:

``` r
dt
#> Source: local data table [5 x 4]
#> Call:   `_DT1`
#> 
#>       a     b     c     d
#>   <int> <int> <int> <int>
#> 1     1     1     1     1
#> 2     2     2     2     2
#> 3     3     3     3     3
#> 4     4     4     4     4
#> 5     5     5     5     5
#> 
#> # Use as.data.table()/as.data.frame()/as_tibble() to access results
```

If we just want to see the generated code, you can use
[`show_query()`](https://dplyr.tidyverse.org/reference/explain.html).
I’ll use that a lot in this vignette.

``` r
dt %>% show_query()
#> `_DT1`
```

## Simple verbs

Many dplyr verbs have a straightforward translation to either the `i` or
`j` component of `[.data.table`.

### `filter()` and `arrange()`

[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) become
elements of `i`:

``` r
dt %>% arrange(a, b, c) %>% show_query()
#> `_DT1`[order(a, b, c)]

dt %>% filter(b == c) %>% show_query()
#> `_DT1`[b == c]
dt %>% filter(b == c, c == d) %>% show_query()
#> `_DT1`[b == c & c == d]
```

### `select()`, `summarise()`, `transmute()`

[`select()`](https://dplyr.tidyverse.org/reference/select.html),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
and
[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
all become elements of `j`:

``` r
dt %>% select(a:b) %>% show_query()
#> `_DT1`[, .(a, b)]
dt %>% summarise(a = mean(a)) %>% show_query()
#> `_DT1`[, .(a = mean(a))]
dt %>% transmute(a2 = a * 2) %>% show_query()
#> copy(`_DT1`)[, `:=`(a2 = a * 2)][, `:=`(c("a", "b", "c", "d"), 
#>     NULL)]
```

[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) also
uses the `j` component with data.table’s special `:=` operator:

``` r
dt %>% mutate(a2 = a * 2, b2 = b * 2) %>% show_query()
#> copy(`_DT1`)[, `:=`(a2 = a * 2, b2 = b * 2)]
```

Note that dplyr will not copy the input data by default, see below for
more details.

[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) allows
to refer to variables that you just created using an “extended `j`”
expression:

``` r
dt %>% mutate(a2 = a * 2, b2 = b * 2, a4 = a2 * 2) %>% show_query()
#> copy(`_DT1`)[, `:=`(c("a2", "b2", "a4"), {
#>     a2 <- a * 2
#>     b2 <- b * 2
#>     a4 <- a2 * 2
#>     .(a2, b2, a4)
#> })]
```

[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
works similarly:

``` r
dt %>% transmute(a2 = a * 2, b2 = b * 2, a4 = a2 * 2) %>% show_query()
#> copy(`_DT1`)[, `:=`(c("a2", "b2", "a4"), {
#>     a2 <- a * 2
#>     b2 <- b * 2
#>     a4 <- a2 * 2
#>     .(a2, b2, a4)
#> })][, `:=`(c("a", "b", "c", "d"), NULL)]
```

## Other calls

Other verbs require calls to other functions:

### `rename()`

[`rename()`](https://dplyr.tidyverse.org/reference/rename.html) uses
[`setnames()`](https://rdatatable.gitlab.io/data.table/reference/setattr.html):

``` r
dt %>% rename(x = a, y = b) %>% show_query()
#> setnames(copy(`_DT1`), c("a", "b"), c("x", "y"))
```

### `distinct()`

[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) uses
[`unique()`](https://rdrr.io/r/base/unique.html):

``` r
dt %>% distinct() %>% show_query()
#> unique(`_DT1`)
dt %>% distinct(a, b) %>% show_query()
#> unique(`_DT1`[, .(a, b)])
dt %>% distinct(a, b, .keep_all = TRUE) %>% show_query()
#> unique(`_DT1`, by = c("a", "b"))
```

[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) on a
computed column uses an intermediate mutate:

``` r
dt %>% distinct(c = a + b) %>% show_query()
#> unique(copy(`_DT1`)[, `:=`(c = a + b)][, `:=`(c("a", "b", "d"
#> ), NULL)])
dt %>% distinct(c = a + b, .keep_all = TRUE) %>% show_query()
#> unique(copy(`_DT1`)[, `:=`(c = a + b)], by = "c")
```

### Joins

Most joins use the `[.data.table` equivalent:

``` r
dt2 <- lazy_dt(data.frame(a = 1))

dt %>% inner_join(dt2, by = "a") %>% show_query()
#> `_DT1`[`_DT2`, on = .(a), nomatch = NULL, allow.cartesian = TRUE]
dt %>% right_join(dt2, by = "a") %>% show_query()
#> `_DT1`[`_DT2`, on = .(a), allow.cartesian = TRUE]
dt %>% left_join(dt2, by = "a") %>% show_query()
#> `_DT2`[`_DT1`, on = .(a), allow.cartesian = TRUE]
dt %>% anti_join(dt2, by = "a") %>% show_query()
#> `_DT1`[!`_DT2`, on = .(a)]
```

But
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
uses [`merge()`](https://rdrr.io/r/base/merge.html)

``` r
dt %>% full_join(dt2, by = "a") %>% show_query()
#> merge(`_DT1`, `_DT2`, all = TRUE, by.x = "a", by.y = "a", allow.cartesian = TRUE)
```

In some case extra calls to
[`data.table::setcolorder()`](https://rdatatable.gitlab.io/data.table/reference/setcolorder.html)
and
[`data.table::setnames()`](https://rdatatable.gitlab.io/data.table/reference/setattr.html)
are required to ensure correct column order and names in:

``` r
dt3 <- lazy_dt(data.frame(b = 1, a = 1))

dt %>% left_join(dt3, by = "a") %>% show_query()
#> setnames(setcolorder(`_DT3`[`_DT1`, on = .(a), allow.cartesian = TRUE], 
#>     c(2L, 3L, 4L, 5L, 1L)), c("i.b", "b"), c("b.x", "b.y"))
dt %>% full_join(dt3, by = "b") %>% show_query()
#> setcolorder(merge(`_DT1`, `_DT3`, all = TRUE, by.x = "b", by.y = "b", 
#>     allow.cartesian = TRUE), c(2L, 1L, 3L, 4L, 5L))
```

Semi-joins are little more complex:

``` r
dt %>% semi_join(dt2, by = "a") %>% show_query()
#> `_DT1`[unique(`_DT1`[`_DT2`, which = TRUE, nomatch = NULL, on = .(a)])]
```

### Set operations

Set operations use the fast data.table alternatives:

``` r
dt %>% intersect(dt2) %>% show_query()
#> fintersect(`_DT1`, `_DT2`)
dt %>% setdiff(dt2) %>% show_query()
#> fsetdiff(`_DT1`, `_DT2`)
dt %>% union(dt2) %>% show_query()
#> funion(`_DT1`, `_DT2`)
```

## Grouping

Just like in dplyr,
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
doesn’t do anything by itself, but instead modifies the operation of
downstream verbs. This generally just involves using the `keyby`
argument:

``` r
dt %>% group_by(a) %>% summarise(b = mean(b)) %>% show_query()
#> `_DT1`[, .(b = mean(b)), keyby = .(a)]
```

You may use `by` instead of `keyby` if you set `arrange = FALSE`:

``` r
dt %>% group_by(a, arrange = FALSE) %>% summarise(b = mean(b)) %>% show_query()
#> `_DT1`[, .(b = mean(b)), by = .(a)]
```

Often, there won’t be too much of a difference between these, but for
larger grouped operations, the overhead of reordering data may become
significant. In these situations, using `arrange = FALSE` becomes
preferable.

The primary exception is grouped
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html), which
requires the use of `.SD`:

``` r
dt %>% group_by(a) %>% filter(b < mean(b)) %>% show_query()
#> `_DT1`[`_DT1`[, .I[b < mean(b)], by = .(a)]$V1]
```

## Combinations

dtplyr tries to generate generate data.table code as close as possible
to what you’d write by hand, as this tends to unlock data.table’s
tremendous speed. For example, if you
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and then
[`select()`](https://dplyr.tidyverse.org/reference/select.html), dtplyr
generates a single `[`:

``` r
dt %>% 
  filter(a == 1) %>% 
  select(-a) %>% 
  show_query()
#> `_DT1`[a == 1, .(b, c, d)]
```

And similarly when combining filtering and summarising:

``` r
dt %>% 
  group_by(a) %>% 
  filter(b < mean(b)) %>% 
  summarise(c = max(c)) %>% 
  show_query()
#> `_DT1`[`_DT1`[, .I[b < mean(b)], by = .(a)]$V1, .(c = max(c)), 
#>     keyby = .(a)]
```

This is particularly nice when joining two tables together because you
can select variables after you have joined and data.table will only
carry those into the join:

``` r
dt3 <- lazy_dt(data.frame(x = 1, y = 2))
dt4 <- lazy_dt(data.frame(x = 1, a = 2, b = 3, c = 4, d = 5, e = 7))

dt3 %>% 
  left_join(dt4) %>% 
  select(x, a:c) %>% 
  show_query()
#> Joining, by = "x"
#> setcolorder(`_DT5`[`_DT4`, on = .(x), allow.cartesian = TRUE], 
#>     c(1L, 7L, 2L, 3L, 4L, 5L, 6L))[, `:=`(c("y", "d", "e"), NULL)]
```

Note, however, that
[`select()`](https://dplyr.tidyverse.org/reference/select.html)ing and
then [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)ing
must generate two separate calls to `[`, because data.table evaluates
`i` before `j`.

``` r
dt %>% 
  select(X = a, Y = b) %>% 
  filter(X == 1) %>% 
  show_query()
#> `_DT1`[, .(X = a, Y = b)][X == 1]
```

Similarly, a
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) can’t be
combined because `dt[a == 1, .(b2 := b * 2)]` would modify the selected
rows in place:

``` r
dt %>% 
  filter(a == 1) %>% 
  mutate(b2 = b * 2) %>% 
  show_query()
#> `_DT1`[a == 1][, `:=`(b2 = b * 2)]
```

## Copies

By default dtplyr avoids mutating the input data, automatically creating
a
[`copy()`](https://rdatatable.gitlab.io/data.table/reference/copy.html)
if needed:

``` r
dt %>% mutate(a2 = a * 2, b2 = b * 2) %>% show_query()
#> copy(`_DT1`)[, `:=`(a2 = a * 2, b2 = b * 2)]
```

Note that dtplyr does its best to avoid needless copies, so it won’t
explicitly copy if there’s already an implicit copy produced by `[`,
[`head()`](https://rdrr.io/r/utils/head.html),
[`merge()`](https://rdrr.io/r/base/merge.html) or similar:

``` r
dt %>% 
  filter(x == 1) %>% 
  mutate(a2 = a * 2, b2 = b * 2) %>% 
  show_query()
#> `_DT1`[x == 1][, `:=`(a2 = a * 2, b2 = b * 2)]
```

You can choose to opt out of this copy, and take advantage of
data.table’s reference semantics (see
[`vignette("datatable-reference-semantics")`](https://rdatatable.gitlab.io/data.table/articles/datatable-reference-semantics.html)
for more details). Do this by setting `immutable = FALSE` on
construction:

``` r
dt2 <- data.table(a = 1:10)

dt_inplace <- lazy_dt(dt2, immutable = FALSE)
dt_inplace %>% mutate(a2 = a * 2, b2 = b * 2) %>% show_query()
#> `_DT6`[, `:=`(a2 = a * 2, b2 = b * 2)]
```

## Performance

There are two components to the performance of dtplyr: how long it takes
to generate the translation, and how well the translation performs.
Given my explorations so far, I’m reasonably confident that we’re
generating high-quality data.table code, so most of the cost should be
in the translation itself.

The following code briefly explores the performance of a few different
translations. A significant amount of work is done by the dplyr verbs,
so we benchmark the whole process.

``` r
bench::mark(
  filter = dt %>% filter(a == b, c == d),
  mutate = dt %>% mutate(a = a * 2, a4 = a2 * 2, a8 = a4 * 2) %>% show_query(),
  summarise = dt %>% group_by(a) %>% summarise(b = mean(b)) %>% show_query(),
  check = FALSE
)[1:6]
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 filter     732.17µs  779.2µs     1275.    3.25KB     22.3
#> 2 mutate       1.15ms   1.18ms      843.   20.51KB     25.9
#> 3 summarise    1.88ms   1.93ms      515.   29.61KB     21.7
```

These translations all take less than a millisecond, suggesting that the
performance overhead of dtplyr should be negligible for realistic data
sizes. Note that dtplyr run-time scales with the complexity of the
pipeline, not the size of the data, so these timings should apply
regardless of the size of the underlying data[¹](#fn1).

------------------------------------------------------------------------

1.  Unless a copy is performed.
