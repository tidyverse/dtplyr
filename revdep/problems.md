# rFIA

<details>

* Version: 0.3.2
* GitHub: NA
* Source code: https://github.com/cran/rFIA
* Date/Publication: 2021-06-10 15:20:02 UTC
* Number of recursive dependencies: 78

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
    > 
    > ## Most recents subset
    > fiaRI_mr <- clipFIA(fiaRI)
    > 
    > ## Most recent estimates of forested area in RI
    > area(db = fiaRI_mr)
    Error in fifelse(sum(aDI > 0, na.rm = TRUE), 1, 0) : 
      Argument 'test' must be logical.
    Calls: area -> lapply -> FUN
    Execution halted
    ```

