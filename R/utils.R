cat_line <- function(...) cat(paste(..., "\n", collapse = "", sep = ""))

imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}

map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f, env = global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}

strip_names <- function(df, base, names_sep) {
  base <- paste0(base, names_sep)
  names <- names(df)

  has_prefix <- startsWith(names, base)
  names[has_prefix] <- substr(names[has_prefix], nchar(base) + 1, nchar(names[has_prefix]))

  set_names(df, names)
}
