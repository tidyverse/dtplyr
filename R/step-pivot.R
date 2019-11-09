#' Pivot data from long to wide
#' @param data A data frame to pivot.
#' @param cols Columns to pivot into longer format. This takes a tidyselect
#'   specification.
#' @param names_to A string specifying the name of the column to create
#'   from the data stored in the column names of `data`.
#'   Can be a character vector, creating multiple columns, if `names_sep`
#'   or `names_pattern` is provided.
#' @param names_prefix Not supported by data.table backed tables.
#' @param names_sep,names_pattern If `names_to` contains multiple values,
#'   these arguments control how the column name is broken up.
#'
#'   `names_sep` takes the same specification as [separate()], and can either
#'   be a numeric vector (specifying positions to break on), or a single string
#'   (specifying a regular expression to split on).
#'
#'   `names_pattern` takes the same specification as [extract()], a regular
#'   expression containing matching groups (`()`).
#'
#'   If these arguments do not give you enough control, use
#'   `pivot_longer_spec()` to create a spec object and process manually as
#'   needed.
#' @param names_repair What happen if the output has invalid column names?
#'   The default, `"check_unique"` is to error if the columns are duplicated.
#'   Use `"minimal"` to allow duplicates in the output, or `"unique"` to
#'   de-duplicated by adding numeric suffixes. See [vctrs::vec_as_names()]
#'   for more options.
#' @param id_cols A set of columns that uniquely identifies each observation.
#'   Defaults to all columns in `data` except for the columns specified in
#'   `names_from` and `values_from`. Typically used when you have additional
#'   variables that is directly related.
#' @param names_from,values_from A pair of arguments describing which column
#'   (or columns) to get the name of the output column (`name_from`), and
#'   which column (or columns) to get the cell values from (`values_from`).
#'
#'   If `values_from` contains multiple values, the value will be added to the
#'   front of the output column.
#' @param names_sep If `names_from` or `values_from` contains multiple
#'   variables, this will be used to join their values together into a single
#'   string to use as a column name.
#' @param names_prefix String added to the start of every variable name. This is
#'   particularly useful if `names_from` is a numeric vector and you want to
#'   create syntactic variable names.
#' @param values_fill Optionally, a named list specifying what each `value`
#'   should be filled in with when missing.
#' @param values_fn Optionally, a named list providing a function that will be
#'   applied to the `value` in each cell in the output. You will typically
#'   use this when the combination of `id_cols` and `value` column does not
#'   uniquely identify an observation.

#' @export
dt_pivot_wider <- function(data, id_cols = NULL, names_from = NULL, names_prefix = "",
                           names_sep = "_", names_repair = "check_unique",
                           values_from = NULL, values_fill = NULL, values_fn = NULL) {
  if(names_prefix != "") stop("`names_prefix` is not supported by dt_pivot_wider()")
  cn <- data$vars
  names_from <- tidyselect::vars_select(cn, !!enquo(names_from))
  values_from <- tidyselect::vars_select(cn, !!enquo(values_from))
  pl <- c(values_from, names_from)
  pivot_on_vars <- cn[!(cn %in% pl)]
  step_pivot_wider(
    data,
    data$vars,
    data$groups,
    id_cols = id_cols,
    names_from = names_from,
    names_prefix = "",
    names_sep = names_sep,
    names_repair = names_repair,
    values_from = values_from,
    values_fill = values_fill,
    values_fn = values_fn,
    pivot_on_vars = pivot_on_vars
  )
}
step_pivot_wider <- function(parent, new_vars = list(), nested = FALSE, id_cols = NULL,
                             names_from = NULL, names_prefix = "", names_sep = "_",
                             names_repair = "check_unique", values_from = NULL,
                             values_fill = NULL, values_fn = NULL, pivot_on_vars = NULL) {
  vars <- union(parent$vars, names(new_vars))

  new_step(
    parent,
    vars = vars,
    new_vars = new_vars,
    nested = nested,
    id_cols = id_cols,
    names_from = names_from,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_repair = names_repair,
    values_from = values_from,
    values_fill = values_fill,
    values_fn = values_fn,
    pivot_on_vars = pivot_on_vars,
    class = "dtplyr_step_pivot_wider"
  )
}

dt_call.dtplyr_step_pivot_wider <- function(x, needs_copy = x$needs_copy) {
  l_formula <- str2lang(paste0(x$pivot_on_vars, collapse = "+"))
  r_formula <- str2lang(x$names_from)
  p_formula <- new_formula(l_formula, r_formula)

  out <- call2("dcast",
    data = dt_call(x$parent, x$needs_copy),
    formula = p_formula,
    value.var = x$values_from,
    fill = x$values_fill,
    sep = x$names_sep
  )
  out
}

