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
#' library(dplyr)
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
  if (is.grouped_dt(data)) return(ungroup(data))

  if (data.table::is.data.table(data)) {
    if (copy)
      data <- data.table::copy(data)
  } else {
    data <- data.table::as.data.table(data)
  }
  data.table::setattr(data, "class", c("tbl_dt", "tbl", "data.table", "data.frame"))
  data
}

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
.datatable.aware <- TRUE

# Filter -----------------------------------------------------------------------

and_expr <- function(exprs) {
  stopifnot(is.list(exprs))
  if (length(exprs) == 0) return(TRUE)
  if (length(exprs) == 1) return(exprs[[1]])

  left <- exprs[[1]]
  for (i in 2:length(exprs)) {
    left <- substitute(left & right, list(left = left, right = exprs[[i]]))
  }
  left
}

#' @importFrom dplyr filter_
#' @export
filter_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
filter_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
filter_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  env <- lazyeval::common_env(dots)

  # http://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
  expr <- lapply(dots, `[[`, "expr")
  j <- substitute(list(`_row` = .I[expr]), list(expr = and_expr(expr)))
  indices <- dt_subset(.data, , j, env)$`_row`

  .data[indices[!is.na(indices)]]
}

# Summarise --------------------------------------------------------------------

#' @importFrom dplyr summarise_
#' @export
summarise_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), drop_last(groups(.data)), copy = FALSE)
}

#' @export
summarise_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
summarise_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  j <- lazyeval::make_call(quote(list), dots)
  dt_subset(.data, , j$expr, env = j$env)
}

# Mutate -----------------------------------------------------------------------

#' @importFrom dplyr mutate_
#' @export
mutate_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), drop_last(groups(.data)), copy = FALSE)
}

#' @export
mutate_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
mutate_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  names <- lapply(names(dots), as.name)

  # Never want to modify in place
  .data <- data.table::copy(.data)

  for(i in seq_along(dots)) {
    # For each new variable, generate a call of the form df[, new := expr]
    j <- substitute(lhs := rhs, list(lhs = names[[i]], rhs = dots[[i]]$expr))
    .data <- dt_subset(.data, , j, dots[[i]]$env)
  }

  .data
}

# Arrange ----------------------------------------------------------------------

#' @importFrom dplyr arrange_
#' @export
arrange_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
arrange_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
arrange_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  groups <- lazyeval::as.lazy_dots(groups(.data),
    env = lazyeval::common_env(dots))
  i <- lazyeval::make_call(quote(order), c(groups, dots))

  dt_subset(.data, i$expr, , env = i$env)
}

# Select -----------------------------------------------------------------------

#' @importFrom dplyr select_
#' @export
select_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::select_vars_(names(.data), dots,
    include = as.character(groups(.data)))
  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))

  grouped_dt(out, groups(.data), copy = FALSE)
}

#' @export
select_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::select_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))
  out
}

#' @export
select_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# Rename -----------------------------------------------------------------------

#' @importFrom dplyr rename_
#' @export
rename_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::rename_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))

  grouped_dt(out, groups(.data), copy = FALSE)
}

#' @export
rename_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::rename_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))
  out
}

#' @export
rename_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}


# Slice -------------------------------------------------------------------

#' @importFrom dplyr slice_
#' @export
slice_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
slice_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
slice_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  env <- lazyeval::common_env(dots)

  j <- substitute(.SD[rows], list(rows = dots[[1]]$expr))
  dt_subset(.data, , j, env)
}
