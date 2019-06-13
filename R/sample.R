#' @export
sample_n.tbl_dt <- function(tbl, size, replace = FALSE, weight = NULL,
                            .env = parent.frame()) {
  tbl_dt(NextMethod())
}

#' @export
#' @importFrom dplyr sample_n
sample_n.grouped_dt <- function(tbl, size, replace = FALSE, weight = NULL,
                                .env = parent.frame()) {

  idx_call <- substitute(
    list(`row_` = .I[sample(.N, size = size, replace = replace, prob = weight)]),
    list(size = size, replace = replace, weight = substitute(weight))
  )
  idx <- dt_subset(tbl, , idx_call, env = .env)$row_

  grouped_dt(tbl[idx], groups(tbl))
}

#' @importFrom dplyr sample_frac
#' @export
sample_frac.tbl_dt <- function(tbl, size = 1, replace = FALSE, weight = NULL,
                               .env = parent.frame()) {
  tbl_dt(NextMethod())
}

#' @export
sample_frac.grouped_dt <- function(tbl, size = 1, replace = FALSE, weight = NULL,
                                   .env = parent.frame()) {

  idx_call <- substitute(
    list(`row_` = .I[sample(.N, size = round(size * .N), replace = replace, prob = weight)]),
    list(size = size, replace = replace, weight = substitute(weight))
  )
  idx <- dt_subset(tbl, , idx_call, env = .env)$row_

  grouped_dt(tbl[idx], groups(tbl))
}
