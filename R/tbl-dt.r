#' Create a data table tbl.
#'
#' A data table tbl wraps a local data table.
#'
#' @export
#' @param data a data table
#' @param copy If the input is a data.table, copy it?
#' @aliases .datatable.aware
#' @examples
#' ds <- tbl_dt(mtcars)
#' ds
#' data.table::as.data.table(ds)
#'
#' library(dplyr, warn.conflicts = FALSE)
#' if (require("nycflights13")) {
#' flights2 <- tbl_dt(flights)
#' flights2 %>% filter(month == 1, day == 1, dest == "DFW")
#' flights2 %>% select(year:day)
#' flights2 %>% rename(Year = year)
#' flights2 %>%
#'   summarise(
#'     delay = mean(arr_delay, na.rm = TRUE),
#'     n = length(arr_delay)
#'   )
#' flights2 %>%
#'   mutate(gained = arr_delay - dep_delay) %>%
#'   select(ends_with("delay"), gained)
#' flights2 %>%
#'   arrange(dest, desc(arr_delay))
#'
#' by_dest <- group_by(flights2, dest)
#'
#' filter(by_dest, arr_delay == max(arr_delay, na.rm = TRUE))
#' summarise(by_dest, arr = mean(arr_delay, na.rm = TRUE))
#'
#' # Normalise arrival and departure delays by airport
#' by_dest %>%
#'   mutate(arr_z = scale(arr_delay), dep_z = scale(dep_delay)) %>%
#'   select(starts_with("arr"), starts_with("dep"))
#'
#' arrange(by_dest, desc(arr_delay))
#' select(by_dest, -(day:tailnum))
#' rename(by_dest, Year = year)
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # which removes a grouping level
#' by_day <- group_by(flights2, year, month, day)
#' by_month <- summarise(by_day, delayed = sum(arr_delay > 0, na.rm = TRUE))
#' by_month
#' summarise(by_month, delayed = sum(delayed))
#'
#' # You can also manually ungroup:
#' ungroup(by_day)
#' }
tbl_dt <- function(data, copy = TRUE) {
  if (is.grouped_dt(data)) {
    return(ungroup(data))
  }

  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  } else if (copy) {
    data <- data.table::copy(data)
  }

  data.table::setattr(data, "class", c("tbl_dt", "tbl", "data.table", "data.frame"))
  data
}
setOldClass(c("tbl_dt", "tbl", "data.table", "data.frame"))

#' @export
#' @importFrom dplyr as.tbl
as.tbl.data.table <- function(x, ...) {
  tbl_dt(x)
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_dt <- function(x) data.table::copy(names(x))

#' @importFrom dplyr groups
#' @export
groups.tbl_dt <- function(x) {
  NULL
}

#' @importFrom dplyr ungroup
#' @export
ungroup.tbl_dt <- function(x, ...) x

#' @export
ungroup.data.table <- function(x, ...) x

#' @importFrom dplyr same_src
#' @export
same_src.tbl_dt <- function(x, y) {
  data.table::is.data.table(y)
}

#' @importFrom dplyr auto_copy
#' @export
auto_copy.tbl_dt <- function(x, y, copy = FALSE, ...) {
  data.table::as.data.table(as.data.frame(y))
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_dt <- function(x, row.names = NULL, optional = FALSE, ...) {
  NextMethod()
}

#' @export
print.tbl_dt <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data table ", dplyr::dim_desc(x), "\n", sep = "")
  cat("\n")
  print(dplyr::trunc_mat(x, n = n, width = width))

  invisible(x)
}

#' @export
dimnames.tbl_dt <- function(x) data.table::copy(NextMethod())

#' @importFrom utils head
#' @export
head.tbl_dt <- function(x, ...) tbl_dt(NextMethod())

#' @importFrom utils tail
#' @export
tail.tbl_dt <- function(x, ...) tbl_dt(NextMethod())

#' @export
#' @method all.equal tbl_dt
all.equal.tbl_dt <- function(target, current, ignore_col_order = TRUE,
                             ignore_row_order = TRUE, convert = FALSE, ...) {
  dplyr::all_equal(target, current, ignore_col_order = ignore_col_order,
    ignore_row_order = ignore_row_order, convert = convert, ...)
}

#' @export
.datatable.aware <- TRUE

# third version
common_env <- function (dots){
  stopifnot(inherits(dots, "quosures"))
  if (length(dots) == 0) return(baseenv())
  if (length(dots) == 1){
    env <- get_env(dots[[1]])
  } else {
    # evaluate common env
    envs <- lapply(dots, get_env)

    # use deepest env as a candidate for common env
    env <- envs[[which.max(vapply(envs, env_depth, 0L))]]
    # fail if some environments are not in search path
    if (!all(envs %in% c(env_parents(env), env))){
      stop("Conflicting environment tree. Does this ever arise?")
    }
  }

  # from ?quosure:
  #   Literals are enquosed with the empty environment because they can
  #   be evaluated anywhere.
  # But we don't have `[` in emptyenv, which we need in the data.table call.
  # Changing to baseenv which was also returned in lazyeval::all_dots()
  if (identical(env, emptyenv())){
    env <- baseenv()
  }
  env
}
