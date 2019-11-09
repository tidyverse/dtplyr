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
  needs_copy <- x$needs_copy
  l_formula <- str2lang(paste0(x$pivot_on_vars, collapse = "+"))
  r_formula <- str2lang(x$names_from)
  p_formula <- new_formula(l_formula, r_formula)
  out <- call2("dcast",
    data = dt_call(x$parent, needs_copy),
    formula = p_formula,
    value.var = x$values_from
  )
  out
}

#' @export
dt_pivot_wider <- function(data, id_cols = NULL, names_from = NULL, names_prefix = "",
                           names_sep = "_", names_repair = "check_unique",
                           values_from = NULL, values_fill = NULL, values_fn = NULL) {
  cn <- data$vars
  names_from <- tidyselect::vars_select(cn, !!enquo(names_from))
  values_from <- tidyselect::vars_select(cn, !!enquo(values_from))
  pl <- c(values_from, names_from)
  pivot_on_vars <- cn[!(cn %in% pl)]
  step_pivot_wider(data, data$vars, data$groups,
    values_from = values_from,
    names_from = names_from,
    pivot_on_vars = pivot_on_vars
  )
}
