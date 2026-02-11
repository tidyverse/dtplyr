# Changelog

## dtplyr 1.3.3

- Fix for latest dplyr

## dtplyr 1.3.2

CRAN release: 2025-09-10

- `R CMD check` fixes

### New features

- [`reframe()`](https://dplyr.tidyverse.org/reference/reframe.html) is
  now translated.

- [`consecutive_id()`](https://dplyr.tidyverse.org/reference/consecutive_id.html)
  is now mapped to
  [`data.table::rleid()`](https://rdrr.io/pkg/data.table/man/rleid.html).
  Note: [`rleid()`](https://rdrr.io/pkg/data.table/man/rleid.html) only
  accepts vector inputs and cannot be used with data frame inputs.

- [`case_match()`](https://dplyr.tidyverse.org/reference/case_match.html)
  is now translated to
  [`fcase()`](https://rdrr.io/pkg/data.table/man/fcase.html).

### Minor improvements and bug fixes

- Can use `.data` in
  [`lead()`](https://dplyr.tidyverse.org/reference/lead-lag.html)/[`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html)
  ([\#441](https://github.com/tidyverse/dtplyr/issues/441))

- Can namespace calls to
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html)
  ([\#427](https://github.com/tidyverse/dtplyr/issues/427)).

- `case_when(.default = )` now works.

- `.by` no longer alters grouping in prior steps
  ([\#439](https://github.com/tidyverse/dtplyr/issues/439))

- Arguments to `$` and `[[` calls are no longer prepended with `..`
  ([\#434](https://github.com/tidyverse/dtplyr/issues/434))

- Grouping now works with non-standard column names
  ([\#451](https://github.com/tidyverse/dtplyr/issues/451))

- `print.dtplyr_step()` gains `n`, `max_extra_cols`, and
  `max_footer_lines` args
  ([\#464](https://github.com/tidyverse/dtplyr/issues/464))

- [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  preserves row count and avoids unnecessary copies
  ([\#470](https://github.com/tidyverse/dtplyr/issues/470))

## dtplyr 1.3.1

CRAN release: 2023-03-22

- Fix for failing R CMD check.

- `dtplyr` no longer directly depends on `crayon`.

## dtplyr 1.3.0

CRAN release: 2023-02-24

### Breaking changes

- dplyr and tidyr verbs no longer dispatch to dtplyr translations when
  used directly on data.table objects.
  [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md) must
  now explicitly be called by the user
  ([\#312](https://github.com/tidyverse/dtplyr/issues/312)).

### New features

- [`across()`](https://dplyr.tidyverse.org/reference/across.html) output
  can now be used as a data frame
  ([\#341](https://github.com/tidyverse/dtplyr/issues/341)).

- `.by`/`by` has been implemented for
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html), and
  the [`slice()`](https://dplyr.tidyverse.org/reference/slice.html)
  family ([\#399](https://github.com/tidyverse/dtplyr/issues/399)).

- New translations for
  [`add_count()`](https://dplyr.tidyverse.org/reference/count.html),
  [`pick()`](https://dplyr.tidyverse.org/reference/pick.html)
  ([\#341](https://github.com/tidyverse/dtplyr/issues/341)), and
  [`unite()`](https://tidyr.tidyverse.org/reference/unite.html).

- [`min_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`dense_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
  [`percent_rank()`](https://dplyr.tidyverse.org/reference/percent_rank.html),
  &
  [`cume_dist()`](https://dplyr.tidyverse.org/reference/percent_rank.html)
  are now mapped to their `data.table` equivalents
  ([\#396](https://github.com/tidyverse/dtplyr/issues/396)).

### Performance improvements

- [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) now
  utilizes
  [`setorder()`](https://rdrr.io/pkg/data.table/man/setorder.html) when
  possible for improved performance
  ([\#364](https://github.com/tidyverse/dtplyr/issues/364)).

- [`select()`](https://dplyr.tidyverse.org/reference/select.html) now
  drops columns by reference when possible for improved performance
  ([\#367](https://github.com/tidyverse/dtplyr/issues/367)).

- [`slice()`](https://dplyr.tidyverse.org/reference/slice.html) uses an
  intermediate variable to reduce computation time of row selection
  ([\#377](https://github.com/tidyverse/dtplyr/issues/377)).

### Minor improvements and bug fixes

- dtplyr no longer directly depends on `ellipsis`.

- Chained operations properly prevent modify-by-reference
  ([\#210](https://github.com/tidyverse/dtplyr/issues/210)).

- [`across()`](https://dplyr.tidyverse.org/reference/across.html),
  [`if_any()`](https://dplyr.tidyverse.org/reference/across.html), and
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html)
  evaluate the `.cols` argument in the environment from which the
  function was called.

- [`count()`](https://dplyr.tidyverse.org/reference/count.html) properly
  handles grouping variables
  ([\#356](https://github.com/tidyverse/dtplyr/issues/356)).

- [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) now
  supports use of `.data` pronoun inside in
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  ([\#346](https://github.com/tidyverse/dtplyr/issues/346)).

- [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  now produces output with correctly named columns when a non-default
  value for `suffix` is supplied. Previously the `suffix` argument was
  ignored ([\#382](https://github.com/tidyverse/dtplyr/issues/382)).

- [`if_any()`](https://dplyr.tidyverse.org/reference/across.html) and
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html) now
  work without specifying the `.fns` argument
  ([@mgirlich](https://github.com/mgirlich),
  [\#325](https://github.com/tidyverse/dtplyr/issues/325)) and for a
  list of functions specified in the
  ([@mgirlich](https://github.com/mgirlich),
  [\#335](https://github.com/tidyverse/dtplyr/issues/335)).

- [`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)’s
  `names_glue` now works even when `names_from` contains `NA`s
  ([\#394](https://github.com/tidyverse/dtplyr/issues/394)).

- In
  [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  the `y` table is again coerced to a lazy table if `copy = TRUE`
  ([@mgirlich](https://github.com/mgirlich),
  [\#322](https://github.com/tidyverse/dtplyr/issues/322)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) can
  now use `.keep`.

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)/[`summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
  correctly translates anonymous functions
  ([\#362](https://github.com/tidyverse/dtplyr/issues/362)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)/[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  now supports
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) and
  [`stringr::str_glue()`](https://stringr.tidyverse.org/reference/str_glue.html)
  without specifying `.envir`.

- [`where()`](https://tidyselect.r-lib.org/reference/where.html) now
  clearly errors because dtplyr doesn’t support selection by predicate
  ([\#271](https://github.com/tidyverse/dtplyr/issues/271)).

## dtplyr 1.2.2

CRAN release: 2022-08-20

- Hot patch release to resolve R CMD check failures.

## dtplyr 1.2.1

CRAN release: 2022-01-19

- Fix for upcoming rlang release.

## dtplyr 1.2.0

CRAN release: 2021-12-05

### New authors

[@markfairbanks](https://github.com/markfairbanks),
[@mgirlich](https://github.com/mgirlich), and
[@eutwt](https://github.com/eutwt) are now dtplyr authors in recognition
of their significant and sustained contributions. Along with
[@eutwt](https://github.com/eutwt), they supplied the bulk of the
improvements in this release!

### New features

- dtplyr gains translations for many more tidyr verbs:

  - [`drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
    ([@markfairbanks](https://github.com/markfairbanks),
    [\#194](https://github.com/tidyverse/dtplyr/issues/194))
  - [`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
    ([@markfairbanks](https://github.com/markfairbanks),
    [\#225](https://github.com/tidyverse/dtplyr/issues/225))
  - [`expand()`](https://tidyr.tidyverse.org/reference/expand.html)
    ([@markfairbanks](https://github.com/markfairbanks),
    [\#225](https://github.com/tidyverse/dtplyr/issues/225))
  - [`fill()`](https://tidyr.tidyverse.org/reference/fill.html)
    ([@markfairbanks](https://github.com/markfairbanks),
    [\#197](https://github.com/tidyverse/dtplyr/issues/197))
  - [`pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
    ([@markfairbanks](https://github.com/markfairbanks),
    [\#204](https://github.com/tidyverse/dtplyr/issues/204))
  - [`replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html)
    ([@markfairbanks](https://github.com/markfairbanks),
    [\#202](https://github.com/tidyverse/dtplyr/issues/202))
  - [`nest()`](https://tidyr.tidyverse.org/reference/nest.html)
    ([@mgirlich](https://github.com/mgirlich),
    [\#251](https://github.com/tidyverse/dtplyr/issues/251))
  - [`separate()`](https://tidyr.tidyverse.org/reference/separate.html)
    ([@markfairbanks](https://github.com/markfairbanks),
    [\#269](https://github.com/tidyverse/dtplyr/issues/269))

- [`tally()`](https://dplyr.tidyverse.org/reference/count.html) gains a
  translation ([@mgirlich](https://github.com/mgirlich),
  [\#201](https://github.com/tidyverse/dtplyr/issues/201)).

- [`ifelse()`](https://rdrr.io/r/base/ifelse.html) is mapped to
  [`fifelse()`](https://rdrr.io/pkg/data.table/man/fifelse.html)
  ([@markfairbanks](https://github.com/markfairbanks),
  [\#220](https://github.com/tidyverse/dtplyr/issues/220)).

### Minor improvements and bug fixes

- [`slice()`](https://dplyr.tidyverse.org/reference/slice.html) helpers
  ([`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html) and
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html))
  now accept negative values for `n` and `prop`.

- [`across()`](https://dplyr.tidyverse.org/reference/across.html)
  defaults to
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  when `.cols` isn’t provided
  ([@markfairbanks](https://github.com/markfairbanks),
  [\#231](https://github.com/tidyverse/dtplyr/issues/231)), and handles
  named selections ([@eutwt](https://github.com/eutwt)
  [\#293](https://github.com/tidyverse/dtplyr/issues/293)). It ˜ow
  handles `.fns` arguments in more forms
  ([@eutwt](https://github.com/eutwt)
  [\#288](https://github.com/tidyverse/dtplyr/issues/288)):

  - Anonymous functions, such as `function(x) x + 1`
  - Formulas which don’t require a function call, such as `~ 1`

- `arrange(dt, desc(col))` is translated to `dt[order(-col)]` in order
  to take advantage of data.table’s fast order
  ([@markfairbanks](https://github.com/markfairbanks),
  [\#227](https://github.com/tidyverse/dtplyr/issues/227)).

- [`count()`](https://dplyr.tidyverse.org/reference/count.html) applied
  to data.tables no longer breaks when dtplyr is loaded
  ([@mgirlich](https://github.com/mgirlich),
  [\#201](https://github.com/tidyverse/dtplyr/issues/201)).

- [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  supports use of `T` to specify the default
  ([\#272](https://github.com/tidyverse/dtplyr/issues/272)).

- [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) errors
  for named input, e.g. `filter(dt, x = 1)`
  ([@mgirlich](https://github.com/mgirlich),
  [\#267](https://github.com/tidyverse/dtplyr/issues/267)) and works for
  negated logical columns ([@mgirlich](https://github.com/mgirlich),
  [@211](https://github.com/211)).

- [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  ungroups when no grouping variables are specified
  ([@mgirlich](https://github.com/mgirlich),
  [\#248](https://github.com/tidyverse/dtplyr/issues/248)), and supports
  inline mutation like `group_by(dt, y = x)`
  ([@mgirlich](https://github.com/mgirlich),
  [\#246](https://github.com/tidyverse/dtplyr/issues/246)).

- [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
  named arguments are translated to the correct arguments in
  [`data.table::fifelse()`](https://rdrr.io/pkg/data.table/man/fifelse.html)
  ([@markfairbanks](https://github.com/markfairbanks),
  [\#234](https://github.com/tidyverse/dtplyr/issues/234)).
  [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
  supports `.data` and `.env` pronouns
  ([@markfairbanks](https://github.com/markfairbanks),
  [\#220](https://github.com/tidyverse/dtplyr/issues/220)).

- [`if_any()`](https://dplyr.tidyverse.org/reference/across.html) and
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html)
  default to
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  when `.cols` isn’t provided ([@eutwt](https://github.com/eutwt),
  [\#294](https://github.com/tidyverse/dtplyr/issues/294)).

- [`intersect()`](https://generics.r-lib.org/reference/setops.html)/[`union()`](https://generics.r-lib.org/reference/setops.html)/[`union_all()`](https://dplyr.tidyverse.org/reference/setops.html)/[`setdiff()`](https://generics.r-lib.org/reference/setops.html)
  convert data.table inputs to
  [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md)
  ([\#278](https://github.com/tidyverse/dtplyr/issues/278)).

- [`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html)/[`lead()`](https://dplyr.tidyverse.org/reference/lead-lag.html)
  are translated to
  [`shift()`](https://rdrr.io/pkg/data.table/man/shift.html).

- [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md) keeps
  groups ([@mgirlich](https://github.com/mgirlich),
  [\#206](https://github.com/tidyverse/dtplyr/issues/206)).

- [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  produces the same column order as dplyr
  ([@markfairbanks](https://github.com/markfairbanks),
  [\#139](https://github.com/tidyverse/dtplyr/issues/139)).

- [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  and
  [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  perform a cross join for `by = character()`
  ([@mgirlich](https://github.com/mgirlich),
  [\#242](https://github.com/tidyverse/dtplyr/issues/242)).

- [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  and
  [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  are always translated to the `[.data.table` equivalent. For simple
  merges the translation gets a bit longer but thanks to the simpler
  code base it helps to better handle names in `by` and duplicated
  variables names produced in the data.table join
  ([@mgirlich](https://github.com/mgirlich),
  [\#222](https://github.com/tidyverse/dtplyr/issues/222)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
  [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  work when called without variables
  ([@mgirlich](https://github.com/mgirlich),
  [\#248](https://github.com/tidyverse/dtplyr/issues/248)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) gains
  new experimental arguments `.before` and `.after` that allow you to
  control where the new columns are placed (to match dplyr 1.0.0)
  ([@eutwt](https://github.com/eutwt)
  [\#291](https://github.com/tidyverse/dtplyr/issues/291)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) can
  modify grouping columns (instead of creating another column with the
  same name) ([@mgirlich](https://github.com/mgirlich),
  [\#246](https://github.com/tidyverse/dtplyr/issues/246)).

- [`n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
  is translated to
  [`uniqueN()`](https://rdrr.io/pkg/data.table/man/duplicated.html).

- [`tally()`](https://dplyr.tidyverse.org/reference/count.html) and
  [`count()`](https://dplyr.tidyverse.org/reference/count.html) follow
  the dplyr convention of creating a unique name if the default output
  `name` (n) already exists ([@eutwt](https://github.com/eutwt),
  [\#295](https://github.com/tidyverse/dtplyr/issues/295)).

- [`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  names the columns correctly when `names_from` is a numeric column
  ([@mgirlich](https://github.com/mgirlich),
  [\#214](https://github.com/tidyverse/dtplyr/issues/214)).

- [`pull()`](https://dplyr.tidyverse.org/reference/pull.html) supports
  the `name` argument ([@mgirlich](https://github.com/mgirlich),
  [\#263](https://github.com/tidyverse/dtplyr/issues/263)).

- [`slice()`](https://dplyr.tidyverse.org/reference/slice.html) no
  longer returns excess rows
  ([\#10](https://github.com/tidyverse/dtplyr/issues/10)).

- `slice_*()` functions after
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  are faster ([@mgirlich](https://github.com/mgirlich),
  [\#216](https://github.com/tidyverse/dtplyr/issues/216)).

- [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html)
  works when ordering by a character column
  ([@mgirlich](https://github.com/mgirlich),
  [\#218](https://github.com/tidyverse/dtplyr/issues/218)).

- [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  supports the `.groups` argument
  ([@mgirlich](https://github.com/mgirlich),
  [\#245](https://github.com/tidyverse/dtplyr/issues/245)).

- [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
  [`tally()`](https://dplyr.tidyverse.org/reference/count.html), and
  [`count()`](https://dplyr.tidyverse.org/reference/count.html) can
  change the value of a grouping variables
  ([@eutwt](https://github.com/eutwt),
  [\#295](https://github.com/tidyverse/dtplyr/issues/295)).

- [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  doesn’t produce duplicate columns when assigning to the same variable
  ([@mgirlich](https://github.com/mgirlich),
  [\#249](https://github.com/tidyverse/dtplyr/issues/249)). It correctly
  flags grouping variables so they selected
  ([@mgirlich](https://github.com/mgirlich),
  [\#246](https://github.com/tidyverse/dtplyr/issues/246)).

- [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
  removes variables in `...` from grouping
  ([@mgirlich](https://github.com/mgirlich),
  [\#253](https://github.com/tidyverse/dtplyr/issues/253)).

## dtplyr 1.1.0

CRAN release: 2021-02-20

### New features

- All verbs now have (very basic) documentation pointing back to the
  dplyr generic, and providing a (very rough) description of the
  translation accompanied with a few examples.

- Passing a data.table to a dplyr generic now converts it to a
  [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md),
  making it a little easier to move between data.table and dplyr syntax.

- dtplyr has been bought up to compatibility with dplyr 1.0.0. This
  includes new translations for:

  - [`across()`](https://dplyr.tidyverse.org/reference/across.html),
    [`if_any()`](https://dplyr.tidyverse.org/reference/across.html),
    [`if_all()`](https://dplyr.tidyverse.org/reference/across.html)
    ([\#154](https://github.com/tidyverse/dtplyr/issues/154)).

  - [`count()`](https://dplyr.tidyverse.org/reference/count.html)
    ([\#159](https://github.com/tidyverse/dtplyr/issues/159)).

  - [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
    ([@smingerson](https://github.com/smingerson),
    [\#162](https://github.com/tidyverse/dtplyr/issues/162)).

  - [`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html)
    ([\#160](https://github.com/tidyverse/dtplyr/issues/160))

  - [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
    [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html),
    [`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html),
    [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html),
    and
    [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
    ([\#174](https://github.com/tidyverse/dtplyr/issues/174)).

  And [`rename()`](https://dplyr.tidyverse.org/reference/rename.html)
  and [`select()`](https://dplyr.tidyverse.org/reference/select.html)
  now support dplyr 1.0.0 tidyselect syntax (apart from predicate
  functions which can’t easily work on lazily evaluated data tables).

- We have begun the process of adding translations for tidyr verbs
  beginning with
  [`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  ([@markfairbanks](https://github.com/markfairbanks),
  [\#189](https://github.com/tidyverse/dtplyr/issues/189)).

### Translation improvements

- [`compute()`](https://dplyr.tidyverse.org/reference/compute.html) now
  creates an intermediate assignment within the translation. This will
  generally have little impact on performance but it allows you to use
  intermediate variables to simplify complex translations.

- [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  is now translated to
  [`fcase()`](https://rdrr.io/pkg/data.table/man/fcase.html)
  ([\#190](https://github.com/tidyverse/dtplyr/issues/190)).

- [`cur_data()`](https://dplyr.tidyverse.org/reference/deprec-context.html)
  (`.SD`),
  [`cur_group()`](https://dplyr.tidyverse.org/reference/context.html)
  (`.BY`),
  [`cur_group_id()`](https://dplyr.tidyverse.org/reference/context.html)
  (`.GRP`), and `cur_group_rows() (`.I\`) are now tranlsated to their
  data.table equivalents
  ([\#166](https://github.com/tidyverse/dtplyr/issues/166)).

- [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) on
  grouped data nows use a much faster translation using on `.I` rather
  than `.SD` (and requiring an intermediate assignment)
  ([\#176](https://github.com/tidyverse/dtplyr/issues/176)). Thanks to
  suggestion from [@myoung3](https://github.com/myoung3) and
  [@ColeMiller1](https://github.com/ColeMiller1).

- Translation of individual expressions:

  - `x[[1]]` is now translated correctly.

  - Anonymous functions are now preserved
    ([@smingerson](https://github.com/smingerson),
    [\#155](https://github.com/tidyverse/dtplyr/issues/155))

  - Environment variables used in the `i` argument of `[.data.table` are
    now correctly inlined when not in the global environment
    ([\#164](https://github.com/tidyverse/dtplyr/issues/164)).

  - `T` and `F` are correctly translated to `TRUE` and `FALSE`
    ([\#140](https://github.com/tidyverse/dtplyr/issues/140)).

### Minor improvements and bug fixes

- Grouped filter, mutate, and slice no longer affect ordering of output
  ([\#178](https://github.com/tidyverse/dtplyr/issues/178)).

- [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  gains a `.name_repair` argument
  ([@markfairbanks](https://github.com/markfairbanks)).

- [`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html)
  always calls `[]` so that the result will print
  ([\#146](https://github.com/tidyverse/dtplyr/issues/146)).

- `print.lazy_dt()` shows total rows, and grouping, if present.

- [`group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
  and
  [`group_walk()`](https://dplyr.tidyverse.org/reference/group_map.html)
  are now translated
  ([\#108](https://github.com/tidyverse/dtplyr/issues/108)).

## dtplyr 1.0.1

CRAN release: 2020-01-23

- Better handling for `.data` and `.env` pronouns
  ([\#138](https://github.com/tidyverse/dtplyr/issues/138)).

- dplyr verbs now work with `NULL` inputs
  ([\#129](https://github.com/tidyverse/dtplyr/issues/129)).

- joins do better job at determining output variables in the presence of
  duplicated outputs
  ([\#128](https://github.com/tidyverse/dtplyr/issues/128)). When
  joining based on different variables in `x` and `y`, joins
  consistently preserve column from `x`, not `y`
  ([\#137](https://github.com/tidyverse/dtplyr/issues/137)).

- [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md)
  objects now have a useful
  [`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) method
  ([\#132](https://github.com/tidyverse/dtplyr/issues/132)).

- [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  now has an `arrange` parameter which, if set to `FALSE`, sets the
  data.table translation to use `by` rather than `keyby`
  ([\#85](https://github.com/tidyverse/dtplyr/issues/85)).

- [`rename()`](https://dplyr.tidyverse.org/reference/rename.html) now
  works without `data.table` attached, as intended
  ([@michaelchirico](https://github.com/michaelchirico),
  [\#123](https://github.com/tidyverse/dtplyr/issues/123)).

- dtplyr has been re-licensed as MIT
  ([\#165](https://github.com/tidyverse/dtplyr/issues/165)).

## dtplyr 1.0.0

CRAN release: 2019-11-12

- Converted from eager approach to lazy approach. You now must use
  [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md) to
  begin a translation pipeline, and must use
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html),
  [`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), or
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  to finish the translation and actually perform the computation
  ([\#38](https://github.com/tidyverse/dtplyr/issues/38)).

  This represents a complete overhaul of the package replacing the eager
  evaluation used in the previous releases. This unfortunately breaks
  all existing code that used dtplyr, but frankly the previous version
  was extremely inefficient so offered little of data.table’s impressive
  speed, and was used by very few people.

- dtplyr provides methods for data.tables that warning you that they use
  the data frame implementation and you should use
  [`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.md)
  ([\#77](https://github.com/tidyverse/dtplyr/issues/77))

- Joins now pass `...` on to data.table’s merge method
  ([\#41](https://github.com/tidyverse/dtplyr/issues/41)).

- [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html) now
  copies its input ([@christophsax](https://github.com/christophsax),
  [\#54](https://github.com/tidyverse/dtplyr/issues/54)).

- [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  preserves grouping ([@christophsax](https://github.com/christophsax),
  [\#17](https://github.com/tidyverse/dtplyr/issues/17)).

- [`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html) and
  [`coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)
  are mapped to data.table’s
  [`fifelse()`](https://rdrr.io/pkg/data.table/man/fifelse.html) and
  [`fcoalesce()`](https://rdrr.io/pkg/data.table/man/coalesce.html)
  respectively ([@michaelchirico](https://github.com/michaelchirico),
  [\#112](https://github.com/tidyverse/dtplyr/issues/112)).

## dtplyr 0.0.3

CRAN release: 2019-02-25

- Maintenance release for CRAN checks.

- [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  and
  [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html):
  new `suffix` argument which allows you to control what suffix
  duplicated variable names receive, as introduced in dplyr 0.5
  ([\#40](https://github.com/tidyverse/dtplyr/issues/40),
  [@christophsax](https://github.com/christophsax)).

- Joins use extended
  [`merge.data.table()`](https://rdrr.io/pkg/data.table/man/merge.html)
  and the `on` argument, introduced in data.table 1.9.6. Avoids copy and
  allows joins by different keys
  ([\#20](https://github.com/tidyverse/dtplyr/issues/20),
  [\#21](https://github.com/tidyverse/dtplyr/issues/21),
  [@christophsax](https://github.com/christophsax)).

## dtplyr 0.0.2

CRAN release: 2017-04-21

- This is a compatibility release. It makes dtplyr compatible with dplyr
  0.6.0 in addition to dplyr 0.5.0.

## dtplyr 0.0.1

CRAN release: 2016-06-27

- [`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
  gains `.keep_all` argument
  ([\#30](https://github.com/tidyverse/dtplyr/issues/30),
  [\#31](https://github.com/tidyverse/dtplyr/issues/31)).

- Slightly improve test coverage
  ([\#6](https://github.com/tidyverse/dtplyr/issues/6)).

- Install `devtools` from GitHub on Travis
  ([\#32](https://github.com/tidyverse/dtplyr/issues/32)).

- Joins return `data.table`. Right and full join are now implemented
  ([\#16](https://github.com/tidyverse/dtplyr/issues/16),
  [\#19](https://github.com/tidyverse/dtplyr/issues/19)).

- Remove warnings from tests
  ([\#4](https://github.com/tidyverse/dtplyr/issues/4)).

- Extracted from `dplyr` at revision e5f2952923028803.
