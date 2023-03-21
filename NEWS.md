# dtplyr 1.3.1

* Fix for failing R CMD check.

* `dtplyr` no longer directly depends on `crayon`.

# dtplyr 1.3.0

## Breaking changes

* dplyr and tidyr verbs no longer dispatch to dtplyr translations when used
  directly on data.table objects. `lazy_dt()` must now explicitly be called by
  the user (#312).

## New features

* `across()` output can now be used as a data frame (#341).

* `.by`/`by` has been implemented for `mutate()`, `summarise()`, `filter()`,
  and the `slice()` family (#399).

* New translations for `add_count()`, `pick()` (#341), and `unite()`.

* `min_rank()`, `dense_rank()`, `percent_rank()`, & `cume_dist()` are now mapped
  to their `data.table` equivalents (#396).

## Performance improvements

* `arrange()` now utilizes `setorder()` when possible for improved performance 
  (#364).

* `select()` now drops columns by reference when possible for improved 
  performance (#367).

* `slice()` uses an intermediate variable to reduce computation time of row 
  selection (#377).

## Minor improvements and bug fixes

* dtplyr no longer directly depends on `ellipsis`.

* Chained operations properly prevent modify-by-reference (#210).

* `across()`, `if_any()`, and `if_all()` evaluate the `.cols` argument
  in the environment from which the function was called.
 
* `count()` properly handles grouping variables (#356).

* `desc()` now supports use of `.data` pronoun inside in `arrange()` (#346).

* `full_join()` now produces output with correctly named columns when a 
  non-default value for `suffix` is supplied. Previously the `suffix` argument 
  was ignored (#382).

* `if_any()` and `if_all()` now work without specifying the `.fns` argument 
  (@mgirlich, #325) and for a list of functions specified in the 
  (@mgirlich, #335).

* `pivot_wider()`'s `names_glue` now works even when `names_from` contains 
  `NA`s (#394).

* In `semi_join()` the `y` table is again coerced to a lazy table if 
  `copy = TRUE` (@mgirlich, #322).

* `mutate()` can now use `.keep`.

* `mutate()`/`summarize()` correctly translates anonymous functions (#362).

* `mutate()`/`transmute()` now supports `glue::glue()` and `stringr::str_glue()` 
   without specifying  `.envir`.

* `where()` now clearly errors because dtplyr doesn't support selection 
  by predicate (#271).

# dtplyr 1.2.2

* Hot patch release to resolve R CMD check failures.

# dtplyr 1.2.1

* Fix for upcoming rlang release.

# dtplyr 1.2.0

## New authors

@markfairbanks, @mgirlich, and @eutwt are now dtplyr authors in recognition of their significant and sustained contributions. Along with @eutwt, they supplied the bulk of the improvements in this release!

## New features

* dtplyr gains translations for many more tidyr verbs:
  
  * `drop_na()` (@markfairbanks, #194)
  * `complete()` (@markfairbanks, #225)
  * `expand()` (@markfairbanks, #225)
  * `fill()` (@markfairbanks, #197)
  * `pivot_longer()` (@markfairbanks, #204)
  * `replace_na()` (@markfairbanks, #202)
  * `nest()` (@mgirlich, #251)
  * `separate()` (@markfairbanks, #269)

* `tally()` gains a translation (@mgirlich, #201).

* `ifelse()` is mapped to `fifelse()` (@markfairbanks, #220).

## Minor improvements and bug fixes

*  `slice()` helpers (`slice_head()`, `slice_tail()`, `slice_min()`, `slice_max()` 
   and `slice_sample()`) now accept negative values for `n` and `prop`.

* `across()` defaults to `everything()` when `.cols` isn't provided 
  (@markfairbanks, #231), and handles named selections (@eutwt #293).
  It ˜ow handles `.fns` arguments in more forms (@eutwt #288): 

    * Anonymous functions, such as `function(x) x + 1`
    * Formulas which don't require a function call, such as `~ 1`

* `arrange(dt, desc(col))` is translated to `dt[order(-col)]` in order to 
  take advantage of data.table's fast order (@markfairbanks, #227).

* `count()` applied to data.tables no longer breaks when dtplyr is loaded 
  (@mgirlich, #201).
  
* `case_when()` supports use of `T` to specify the default (#272).

* `filter()` errors for named input, e.g. `filter(dt, x = 1)` 
  (@mgirlich, #267) and works for negated logical columns (@mgirlich, @211).

* `group_by()` ungroups when no grouping variables are specified
  (@mgirlich, #248), and supports inline mutation like `group_by(dt, y = x)` 
  (@mgirlich, #246).

* `if_else()` named arguments are translated to the correct arguments in 
  `data.table::fifelse()` (@markfairbanks, #234). `if_else()`
  supports `.data` and `.env` pronouns (@markfairbanks, #220).

* `if_any()` and `if_all()` default to `everything()` when `.cols` isn't
   provided (@eutwt, #294).

* `intersect()`/`union()`/`union_all()`/`setdiff()` convert data.table inputs 
   to `lazy_dt()` (#278).

* `lag()`/`lead()` are translated to `shift()`.

* `lazy_dt()` keeps groups (@mgirlich, #206).

* `left_join()` produces the same column order as dplyr 
  (@markfairbanks, #139).

* `left_join()`, `right_join()`, `full_join()`, and `inner_join()` perform a
  cross join for `by = character()` (@mgirlich, #242).
  
* `left_join()`, `right_join()`, and `inner_join()` are always translated to
  the `[.data.table` equivalent. For simple merges the translation gets a bit
  longer but thanks to the simpler code base it helps to better handle
  names in `by` and duplicated variables names produced in the data.table join
  (@mgirlich, #222).
  
* `mutate()` and `transmute()` work when called without variables 
  (@mgirlich, #248).

* `mutate()` gains new experimental arguments `.before` and `.after` that allow 
   you to control where the new columns are placed (to match dplyr 1.0.0) 
   (@eutwt #291).
  
* `mutate()` can modify grouping columns (instead of creating another 
  column with the same name) (@mgirlich, #246).
  
* `n_distinct()` is translated to `uniqueN()`.

* `tally()` and `count()` follow the dplyr convention of creating a unique 
  name if the default output `name` (n) already exists (@eutwt, #295).

* `pivot_wider()` names the columns correctly when `names_from` is a
  numeric column (@mgirlich, #214).

* `pull()` supports the `name` argument (@mgirlich, #263).

* `slice()` no longer returns excess rows (#10).

* `slice_*()` functions after `group_by()` are faster (@mgirlich, #216).

* `slice_max()` works when ordering by a character column (@mgirlich, #218).

* `summarise()` supports the `.groups` argument (@mgirlich, #245).

* `summarise()`, `tally()`, and `count()` can change the value of a grouping
  variables (@eutwt, #295).

* `transmute()` doesn't produce duplicate columns when assigning to the same
  variable (@mgirlich, #249). It correctly flags grouping variables so they
  selected (@mgirlich, #246).

* `ungroup()` removes variables in `...` from grouping (@mgirlich, #253).

# dtplyr 1.1.0

## New features

* All verbs now have (very basic) documentation pointing back to the
  dplyr generic, and providing a (very rough) description of the translation
  accompanied with a few examples.

* Passing a data.table to a dplyr generic now converts it to a `lazy_dt()`,
  making it a little easier to move between data.table and dplyr syntax.

* dtplyr has been bought up to compatibility with dplyr 1.0.0. This includes
  new translations for:

  * `across()`, `if_any()`, `if_all()` (#154).

  * `count()` (#159).

  * `relocate()` (@smingerson, #162).

  * `rename_with()` (#160)

  * `slice_min()`, `slice_max()`, `slice_head()`, `slice_tail()`, and
    `slice_sample()` (#174).

  And `rename()` and `select()` now support dplyr 1.0.0 tidyselect syntax 
  (apart from predicate functions which can't easily work on lazily evaluated
  data tables).

* We have begun the process of adding translations for tidyr verbs beginning
  with `pivot_wider()` (@markfairbanks, #189).

## Translation improvements

* `compute()` now creates an intermediate assignment within the translation. 
  This will generally have little impact on performance but it allows you to 
  use intermediate variables to simplify complex translations.

* `case_when()` is now translated to `fcase()` (#190).

* `cur_data()` (`.SD`), `cur_group()` (`.BY`), `cur_group_id()` (`.GRP`), 
   and `cur_group_rows() (`.I`) are now tranlsated to their data.table 
   equivalents (#166).

* `filter()` on grouped data nows use a much faster translation using on `.I`
  rather than `.SD` (and requiring an intermediate assignment) (#176). Thanks 
  to suggestion from @myoung3 and @ColeMiller1.

* Translation of individual expressions:

  * `x[[1]]` is now translated correctly.
  
  * Anonymous functions are now preserved (@smingerson, #155)
  
  * Environment variables used in the `i` argument of `[.data.table` are
    now correctly inlined when not in the global environment (#164).

  * `T` and `F` are correctly translated to `TRUE` and `FALSE` (#140).

## Minor improvements and bug fixes

* Grouped filter, mutate, and slice no longer affect ordering of output (#178).

* `as_tibble()` gains a `.name_repair` argument (@markfairbanks).

* `as.data.table()` always calls `[]` so that the result will print (#146). 

* `print.lazy_dt()` shows total rows, and grouping, if present.

* `group_map()` and `group_walk()` are now translated (#108).

# dtplyr 1.0.1

* Better handling for `.data` and `.env` pronouns (#138).

* dplyr verbs now work with `NULL` inputs (#129).

* joins do better job at determining output variables in the presence of 
  duplicated outputs (#128). When joining based on different variables in `x` 
  and `y`, joins consistently preserve column from `x`, not `y` (#137).

* `lazy_dt()` objects now have a useful `glimpse()` method (#132).

* `group_by()` now has an `arrange` parameter which, if set to `FALSE`, sets 
  the data.table translation to use `by` rather than `keyby` (#85).

* `rename()` now works without `data.table` attached, as intended 
  (@michaelchirico, #123).

* dtplyr has been re-licensed as MIT (#165).  

# dtplyr 1.0.0

*   Converted from eager approach to lazy approach. You now must use `lazy_dt()`
    to begin a translation pipeline, and must use `collect()`, `as.data.table()`, 
    `as.data.frame()`, or `as_tibble()` to finish the translation and actually
    perform the computation (#38).
    
    This represents a complete overhaul of the package replacing the eager 
    evaluation used in the previous releases. This unfortunately breaks all
    existing code that used dtplyr, but frankly the previous version was 
    extremely inefficient so offered little of data.table's impressive speed,
    and was used by very few people.

* dtplyr provides methods for data.tables that warning you that they use the
  data frame implementation and you should use `lazy_dt()` (#77)

* Joins now pass `...` on to data.table's merge method (#41).

* `ungroup()` now copies its input (@christophsax, #54).

* `mutate()` preserves grouping (@christophsax, #17).

* `if_else()` and `coalesce()` are mapped to data.table's `fifelse()` and 
  `fcoalesce()` respectively (@michaelchirico, #112).

# dtplyr 0.0.3

- Maintenance release for CRAN checks.

- `inner_join()`, `left_join()`, `right_join()`, and `full_join()`: new `suffix`
  argument which allows you to control what suffix duplicated variable names
  receive, as introduced in dplyr 0.5 (#40, @christophsax).

- Joins use extended `merge.data.table()` and the `on` argument, introduced in
  data.table 1.9.6. Avoids copy and allows joins by different keys (#20, #21,
  @christophsax).

# dtplyr 0.0.2

- This is a compatibility release. It makes dtplyr compatible with
  dplyr 0.6.0 in addition to dplyr 0.5.0.


# dtplyr 0.0.1

- `distinct()` gains `.keep_all` argument (#30, #31).

- Slightly improve test coverage (#6).

- Install `devtools` from GitHub on Travis (#32).

- Joins return `data.table`. Right and full join are now implemented (#16, #19).

- Remove warnings from tests (#4).

- Extracted from `dplyr` at revision e5f2952923028803.
