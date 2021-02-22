#' Fill in missing values with previous or next value
#'
#' @description
#' This is a method for the tidyr `fill()` generic. It is translated to
#' [data.table::nafill()]. Note that `data.table::nafill()` currently only
#' works for integer and double columns.
#'
#' @inheritParams tidyr::fill
#' @examples
#' library(tidyr)
#'
#' # Value (year) is recorded only when it changes
#' sales <- lazy_dt(tibble::tribble(
#'   ~quarter, ~year, ~sales,
#'   "Q1",    2000,    66013,
#'   "Q2",      NA,    69182,
#'   "Q3",      NA,    53175,
#'   "Q4",      NA,    21001,
#'   "Q1",    2001,    46036,
#'   "Q2",      NA,    58842,
#'   "Q3",      NA,    44568,
#'   "Q4",      NA,    50197,
#'   "Q1",    2002,    39113,
#'   "Q2",      NA,    41668,
#'   "Q3",      NA,    30144,
#'   "Q4",      NA,    52897,
#'   "Q1",    2004,    32129,
#'   "Q2",      NA,    67686,
#'   "Q3",      NA,    31768,
#'   "Q4",      NA,    49094
#' ))
#'
#' # `fill()` defaults to replacing missing data from top to bottom
#' sales %>% fill(year)
#'
#' # Value (n_squirrels) is missing above and below within a group
#' squirrels <- lazy_dt(tibble::tribble(
#'   ~group,    ~name,     ~role,     ~n_squirrels,
#'   1,      "Sam",    "Observer",   NA,
#'   1,     "Mara", "Scorekeeper",    8,
#'   1,    "Jesse",    "Observer",   NA,
#'   1,      "Tom",    "Observer",   NA,
#'   2,     "Mike",    "Observer",   NA,
#'   2,  "Rachael",    "Observer",   NA,
#'   2,  "Sydekea", "Scorekeeper",   14,
#'   2, "Gabriela",    "Observer",   NA,
#'   3,  "Derrick",    "Observer",   NA,
#'   3,     "Kara", "Scorekeeper",    9,
#'   3,    "Emily",    "Observer",   NA,
#'   3, "Danielle",    "Observer",   NA
#' ))
#'
#' # The values are inconsistently missing by position within the group
#' # Use .direction = "downup" to fill missing values in both directions
#' squirrels %>%
#'   dplyr::group_by(group) %>%
#'   fill(n_squirrels, .direction = "downup") %>%
#'   dplyr::ungroup()
#'
#' # Using `.direction = "updown"` accomplishes the same goal in this example
# exported onLoad
fill.dtplyr_step <- function(data, ..., .direction = c("down", "up", "downup", "updown")) {

  dots <- enquos(...)

  .direction <- arg_match(.direction)

  if (.direction %in% c("down", "up")) {
    type <- switch(.direction, "down" = "locf", "up" = "nocb")
    mutate(data, dplyr::across(c(!!!dots), nafill, type))
  } else {
    if (.direction == "downup") {
      type1 <- "locf"
      type2 <- "nocb"
    } else {
      type1 <- "nocb"
      type2 <- "locf"
    }

    mutate(data, dplyr::across(c(!!!dots), ~ nafill(nafill(.x, type1), type2)))
  }
}

# exported onLoad
fill.data.table <- function(data, ..., .direction = c("down", "up", "downup", "updown")) {
  data <- lazy_dt(data)
  tidyr::fill(data, ..., .direction = .direction)
}
