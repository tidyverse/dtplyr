# dtplyr

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/dtplyr)](https://cran.r-project.org/package=dtplyr)
[![Travis build status](https://travis-ci.org/tidyverse/dtplyr.svg?branch=master)](https://travis-ci.org/tidyverse/dtplyr)
[![Codecov test coverage](https://codecov.io/gh/tidyverse/dtplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/tidyverse/dtplyr?branch=master)
<!-- badges: end -->

## Overview

dtplyr is the data.table backend for dplyr. It provides S3 methods for data.table objects so that dplyr works the way you expect. 

dtplyr was extracted out of dplyr so it could evolve independently (i.e. more rapidly!) than dplyr. It also makes dplyr a little simpler, and it's easier to keep track of issues by backend.

## Installation

You can install from CRAN with:

```R
install.packages("dtplyr")
```

Or try the development version from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("tidyverse/dtplyr")
```

## Why is dtplyr slower than data.table?

dtplyr will always be a bit slower than data.table, because it creates copies of objects rather than mutating in place (that's the dplyr philosophy). Currently, dtplyr is quite a lot slower than bare data.table because the methods aren't quite smart enough. I hope interested dplyr & data.table users from the community will help me to improve the performance.

* To match base R semantics, `mutate()` never modifies in place; it always
  creates a `copy()` first.
  
* Each dplyr verb must do some computation to convert dplyr syntax to 
  data.table syntax. This takes time proportional to the complexity of 
  the input code, not the input _data_, so should be a negligible overhead
  for large datasets.
  
* Some data.table expressions have no direct dplyr equivalent. For example,
  `X[Y, sum(foo*bar)]` selects the relevant variables _before_ joining. 
  The dplyr equivalent, `X %>% left_join(Y) %>% summarise(sum(foo * bar))`,
  carries along all variables in the join.

## Usage

To use dtplyr, I recommend loading dplyr, dtplyr, and data.table:

```R
library(dplyr)
library(dtplyr)
library(data.table)
```
