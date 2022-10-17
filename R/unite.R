#' Unite multiple columns into one by pasting strings together.
#'
#' @description
#' This is a method for the tidyr `unite()` generic.
#'
#' @inheritParams tidyr::unite
#' @examples
#' library(tidyr)
#'
#' df <- lazy_dt(expand_grid(x = c("a", NA), y = c("b", NA)))
#' df
#'
#' df %>% unite("z", x:y, remove = FALSE)
#'
#' # Separate is almost the complement of unite
#' df %>%
#'   unite("xy", x:y) %>%
#'   separate(xy, c("x", "y"))
#' # (but note `x` and `y` contain now "NA" not NA)
# exported onLoad
unite.dtplyr_step <- function(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  if (is_true(na.rm)) {
    abort("`na.rm` is not implemented in dtplyr")
  }

  .col <- as_name(enquo(col))

  dots <- enquos(...)
  if (length(dots) == 0) {
    .cols <- data$vars
    locs <- seq_along(.cols)
  } else {
    locs <- tidyselect::eval_select(expr(c(!!!dots)), data, allow_rename = FALSE)
    .cols <- data$vars[locs]
  }

  out <- mutate(ungroup(data), !!.col := paste(!!!syms(.cols), sep = sep))

  remove <- is_true(remove)
  if (remove) {
    .drop_cols <- setdiff(.cols, .col)
    out <- select(out, -tidyselect::all_of(.drop_cols))
  }

  group_vars <- data$groups
  if (remove && any(.cols %in% group_vars)) {
    group_vars <- setdiff(group_vars, .cols)
  }
  out <- relocate(out, !!.col, .before = min(locs))

  if (length(group_vars) > 0) {
    out <- group_by(out, !!!syms(group_vars))
  }

  out
}
