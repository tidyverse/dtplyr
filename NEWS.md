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

* `ungroup()` now copies it's input (@christophsax, #54).

* `mutate()` preserves grouping (@christophsax, #17).

* `if_else()` and `coalesce()` are mapped to data.table's `fifelse()` and 
  `fcoalesce()` respectively (@michaelchirico, #112)

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
