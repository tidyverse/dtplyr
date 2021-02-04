# grattan

<details>

* Version: 1.9.0.4
* GitHub: https://github.com/HughParsonage/grattan
* Source code: https://github.com/cran/grattan
* Date/Publication: 2021-01-29 09:40:05 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "grattan")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ● Inflators not useful post-COVID (1)
      ● Issue 50 unresolved (1)
      ● Not yet implemented (2)
      ● On CRAN (77)
      ● identical(date2fy(Sys.Date()), "2019-20") is not TRUE (1)
      ● threshold on https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411 contradicts taper rates (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test_utils.R:10:3): unselect_ ──────────────────────────────────────
      `y` not equal to `z`.
      target is data.table, current is dtplyr_step_group
      
      [ FAIL 1 | WARN 0 | SKIP 92 | PASS 853 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc    1.9Mb
        libs   3.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hutilscpp’
      All declared Imports should be used.
    ```

# immunarch

<details>

* Version: 0.6.5
* GitHub: https://github.com/immunomind/immunarch
* Source code: https://github.com/cran/immunarch
* Date/Publication: 2020-06-14 17:40:06 UTC
* Number of recursive dependencies: 183

Run `cloud_details(, "immunarch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘immunarch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pubRepStatistics
    > ### Title: Statistics of number of public clonotypes for each possible
    > ###   combinations of repertoires
    > ### Aliases: pubRepStatistics
    > 
    > ### ** Examples
    > 
    > data(immdata)
    > immdata$data <- lapply(immdata$data, head, 2000)
    > pr <- pubRep(immdata$data, .verbose=FALSE)
    > pubRepStatistics(pr) %>% vis()
    Error in names(df) <- repaired_names(c(names2(dimnames(x)), n), .name_repair = .name_repair,  : 
      'names' attribute [2] must be the same length as the vector [1]
    Calls: %>% ... vis -> pubRepStatistics -> as_tibble -> as_tibble.table
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        data   4.3Mb
        doc    1.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dbplyr’
      All declared Imports should be used.
    ```

# tidyquery

<details>

* Version: 0.2.1
* GitHub: https://github.com/ianmcook/tidyquery
* Source code: https://github.com/cran/tidyquery
* Date/Publication: 2020-05-09 21:30:02 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "tidyquery")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure (test-dtplyr.R:14:3): Full example #1 returns expected result on dtplyr_step ──
      query("SELECT origin, dest,\n          COUNT(flight) AS num_flts,\n          round(AVG(distance)) AS dist,\n          round(AVG(arr_delay)) AS avg_delay\n          FROM flights_dt\n        WHERE distance BETWEEN 200 AND 300\n          AND air_time IS NOT NULL\n        GROUP BY origin, dest\n        HAVING num_flts > 3000\n        ORDER BY num_flts DESC, avg_delay DESC\n        LIMIT 100;") not equal to `%>%`(...).
      Component "parent": Component "parent": Component "parent": Component "parent": Component "parent": Component "parent": Component "locals": Names: 1 string mismatch
      Component "parent": Component "parent": Component "parent": Component "parent": Component "parent": Component "parent": Component "name": 1 string mismatch
      Component "parent": Component "parent": Component "parent": Component "parent": Component "parent": Component "locals": Names: 1 string mismatch
      Component "parent": Component "parent": Component "parent": Component "parent": Component "parent": Component "i": target, current do not match when deparsed
      Component "parent": Component "parent": Component "parent": Component "parent": Component "locals": Names: 1 string mismatch
      Component "parent": Component "parent": Component "parent": Component "locals": Names: 1 string mismatch
      Component "parent": Component "parent": Component "locals": Names: 1 string mismatch
      Component "parent": Component "locals": Names: 1 string mismatch
      Component "locals": Names: 1 string mismatch
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 217 ]
      Error: Test failures
      Execution halted
    ```

