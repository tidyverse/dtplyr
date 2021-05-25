strip_names <- function(df, base, names_sep) {
  base <- paste0(base, names_sep)
  names <- names(df)

  has_prefix <- regexpr(base, names, fixed = TRUE) == 1L
  names[has_prefix] <- substr(names[has_prefix], nchar(base) + 1, nchar(names[has_prefix]))

  set_names(df, names)
}
