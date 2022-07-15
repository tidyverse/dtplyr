# rFIA

<details>

* Version: 1.0.0
* GitHub: https://github.com/hunter-stanke/rFIA
* Source code: https://github.com/cran/rFIA
* Date/Publication: 2021-12-15 18:10:02 UTC
* Number of recursive dependencies: 82

Run `cloud_details(, "rFIA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rFIA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: area
    > ### Title: Estimate land area from FIADB
    > ### Aliases: area
    > 
    > ### ** Examples
    > 
    > ## Load data from the rFIA package
    ...
     21.               └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
     22.                 └─tidyselect:::as_indices_sel_impl(...)
     23.                   └─tidyselect:::as_indices_impl(x, vars, call = call, strict = strict)
     24.                     └─tidyselect:::chr_as_locations(x, vars, call = call)
     25.                       └─vctrs::vec_as_location(x, n = length(vars), names = vars)
     26.                         └─vctrs (local) `<fn>`()
     27.                           └─vctrs:::stop_subscript_oob(...)
     28.                             └─vctrs:::stop_subscript(...)
     29.                               └─rlang::abort(...)
    Execution halted
    ```

