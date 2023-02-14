# rFIA

<details>

* Version: 1.0.0
* GitHub: https://github.com/hunter-stanke/rFIA
* Source code: https://github.com/cran/rFIA
* Date/Publication: 2021-12-15 18:10:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "rFIA")` for more info

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
     14. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
     15. │               └─tidyselect:::as_indices_sel_impl(...)
     16. │                 └─tidyselect:::as_indices_impl(...)
     17. │                   └─tidyselect:::chr_as_locations(x, vars, call = call, arg = arg)
     18. │                     └─vctrs::vec_as_location(...)
     19. └─vctrs (local) `<fn>`()
     20.   └─vctrs:::stop_subscript_oob(...)
     21.     └─vctrs:::stop_subscript(...)
     22.       └─rlang::abort(...)
    Execution halted
    ```

