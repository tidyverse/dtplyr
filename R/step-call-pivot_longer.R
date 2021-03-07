#' Pivot data from wide to long
#'
#' @description
#' This is a method for the tidyr `pivot_longer()` generic. It is translated to
#' [data.table::melt()]
#'
#' @param data A [lazy_dt()].
#' @inheritParams tidyr::pivot_longer
#' @param names_ptypes,names_transform,values_ptypes,values_transform
#'   Not currently supported by dtplyr.
#' @examples
#' library(tidyr)
#'
#' # Simplest case where column names are character data
#' relig_income_dt <- lazy_dt(relig_income)
#' relig_income_dt %>%
#'   pivot_longer(!religion, names_to = "income", values_to = "count")
#'
#' # Slightly more complex case where columns have common prefix,
#' # and missing missings are structural so should be dropped.
#' billboard_dt <- lazy_dt(billboard)
#' billboard %>%
#'  pivot_longer(
#'    cols = starts_with("wk"),
#'    names_to = "week",
#'    names_prefix = "wk",
#'    values_to = "rank",
#'    values_drop_na = TRUE
#'  )
#'
#' # Multiple variables stored in column names
#' lazy_dt(who) %>%
#'   pivot_longer(
#'     cols = new_sp_m014:newrel_f65,
#'     names_to = c("diagnosis", "gender", "age"),
#'     names_pattern = "new_?(.*)_(.)(.*)",
#'     values_to = "count"
#'   )
#'
#' # Multiple observations per row
#' anscombe_dt <- lazy_dt(anscombe)
#' anscombe_dt %>%
#'  pivot_longer(
#'    everything(),
#'    names_to = c(".value", "set"),
#'    names_pattern = "(.)(.)"
#'  )
# exported onLoad
pivot_longer.dtplyr_step <- function(data,
                                     cols,
                                     names_to = "name",
                                     names_prefix = NULL,
                                     names_sep = NULL,
                                     names_pattern = NULL,
                                     names_ptypes = NULL,
                                     names_transform = NULL,
                                     names_repair = "check_unique",
                                     values_to = "value",
                                     values_drop_na = FALSE,
                                     values_ptypes = NULL,
                                     values_transform = NULL,
                                     ...) {

  if (!is.null(names_ptypes)) {
    abort("`names_ptypes` is not supported by dtplyr")
  }

  if (!is.null(names_transform)) {
    abort("`names_transform` is not supported by dtplyr")
  }

  if (!is.null(values_ptypes)) {
    abort("`values_ptypes` is not supported by dtplyr")
  }

  if (!is.null(values_transform)) {
    abort("`values_transform` is not supported by dtplyr")
  }

  sim_data <- simulate_vars(data)
  measure_vars <- names(tidyselect::eval_select(enquo(cols), sim_data))
  if (length(measure_vars) == 0) {
    abort("`cols` must select at least one column.")
  }

  multiple_names_to <- length(names_to) > 1
  uses_dot_value <- ".value" %in% names_to

  variable_name <- "variable"

  if (uses_dot_value) {
    if (!is.null(names_sep)) {
      .value <- str_separate(measure_vars, into = names_to, sep = names_sep)$.value
    } else if (!is.null(names_pattern)) {
      .value <- str_extract(measure_vars, into = names_to, names_pattern)$.value
    } else {
      abort("If you use '.value' in `names_to` you must also supply
            `names_sep' or `names_pattern")
    }

    v_fct <- factor(.value, levels = unique(.value))
    measure_vars <- split(measure_vars, v_fct)
    values_to <- names(measure_vars)
    names(measure_vars) <- NULL

    if (multiple_names_to) {
      variable_name <- names_to[!names_to == ".value"]
    }
  } else if (multiple_names_to) {
    if (is.null(names_sep) && is.null(names_pattern)) {
      abort("If you supply multiple names in `names_to` you must also
            supply `names_sep` or `names_pattern`")
    } else if (!is.null(names_sep) && !is.null(names_pattern)) {
      abort("only one of names_sep or names_pattern should be provided")
    }
  } else {
    variable_name <- names_to
  }

  args <- list(
    measure.vars = measure_vars,
    variable.name = variable_name,
    value.name = values_to,
    na.rm = values_drop_na,
    variable.factor = FALSE
  )

  # Clean up call args if defaults are used
  if (variable_name == "variable") {
    args$variable.name <- NULL
  }

  if (identical(values_to, "value")) {
    args$value.name <- NULL
  }

  if (is_false(values_drop_na)) {
    args$na.rm <- NULL
  }

  sim_vars <- names(sim_data)
  id_vars <- sim_vars[!sim_vars %in% unlist(measure_vars)]

  out <- step_call(
    data,
    "melt",
    args = args,
    vars = c(id_vars, variable_name, values_to)
  )

  if (!is.null(names_prefix)) {
    out <- mutate(out, !!variable_name := gsub(paste0("^", names_prefix), "", !!sym(variable_name)))
  }

  if (multiple_names_to && !uses_dot_value) {
    if (!is.null(names_sep)) {
      into_cols <- str_separate(pull(out, !!sym(variable_name)), names_to, sep = names_sep)
    } else {
      into_cols <- str_extract(pull(out, !!sym(variable_name)), into = names_to, regex = names_pattern)
    }
    out <- mutate(out, !!!into_cols)

    # Need to drop variable_name and move names_to vars to correct position
    # Recreates relocate logic so only select is necessary, not relocate + select
    out_vars <- out$vars
    var_idx <- which(out_vars == variable_name)
    before_vars <- out_vars[seq_along(out_vars) < var_idx]
    after_vars <- out_vars[seq_along(out_vars) > var_idx]

    out <- select(out, !!!syms(before_vars), !!!syms(names_to), !!!syms(after_vars))
  } else if (!multiple_names_to && uses_dot_value) {
    out <- mutate(out, variable = NULL)
  }

  step_repair(out, repair = names_repair)
}

# exported onLoad
pivot_longer.data.table <- function(data,
                                    cols,
                                    names_to = "name",
                                    names_prefix = NULL,
                                    names_sep = NULL,
                                    names_pattern = NULL,
                                    names_ptypes = NULL,
                                    names_transform = NULL,
                                    names_repair = "check_unique",
                                    values_to = "value",
                                    values_drop_na = FALSE,
                                    values_ptypes = NULL,
                                    values_transform = NULL,
                                    ...) {
  data <- lazy_dt(data)
  tidyr::pivot_longer(
    data = data,
    cols = {{ cols }},
    names_to = names_to,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_pattern = names_pattern,
    names_ptypes = names_ptypes,
    names_transform = names_transform,
    names_repair = names_repair,
    values_to = values_to,
    values_drop_na = values_drop_na,
    values_ptypes = values_ptypes,
    values_transform = values_transform,
    ...
  )
}

# str_extract()  -----------------------------------------------------------------
str_extract <- function(x, into, regex, convert = FALSE) {
  stopifnot(
    is_string(regex),
    is_character(into)
  )

  out <- str_match_first(x, regex)
  if (length(out) != length(into)) {
    stop(
      "`regex` should define ", length(into), " groups; ", ncol(out), " found.",
      call. = FALSE
    )
  }

  # Handle duplicated names
  if (anyDuplicated(into)) {
    pieces <- split(out, into)
    into <- names(pieces)
    out <- lapply(pieces, pmap_chr, paste0, sep = "")
  }

  into <- as_utf8_character(into)

  non_na_into <- !is.na(into)
  out <- out[non_na_into]
  names(out) <- into[non_na_into]

  if (convert) {
    out[] <- lapply(out, utils::type.convert, as.is = TRUE)
  }

  out
}

str_match_first <- function(string, regex) {
  loc <- regexpr(regex, string, perl = TRUE)
  loc <- group_loc(loc)

  out <- lapply(
    seq_len(loc$matches),
    function(i) substr(string, loc$start[, i], loc$end[, i])
  )
  out[-1]
}

group_loc <- function(x) {
  start <- cbind(as.vector(x), attr(x, "capture.start"))
  end <- start + cbind(attr(x, "match.length"), attr(x, "capture.length")) - 1L

  no_match <- start == -1L
  start[no_match] <- NA
  end[no_match] <- NA

  list(matches = ncol(start), start = start, end = end)
}

# str_separate()  -----------------------------------------------------------------

str_separate <- function(x, into, sep, convert = FALSE, extra = "warn", fill = "warn") {
  if (!is.character(into)) {
    abort("`into` must be a character vector")
  }

  if (is.numeric(sep)) {
    out <- strsep(x, sep)
  } else if (is_character(sep)) {
    out <- data.table::tstrsplit(x, sep, fixed = TRUE, names = TRUE)
    out <- as_tibble(out)
  } else {
    abort("`sep` must be either numeric or character")
  }

  names(out) <- as_utf8_character(into)
  out <- out[!is.na(names(out))]
  if (convert) {
    out[] <- lapply(out, utils::type.convert, as.is = TRUE)
  }
  out
}

strsep <- function(x, sep) {
  nchar <- nchar(x)
  pos <- lapply(sep, function(i) {
    if (i >= 0) return(i)
    pmax(0, nchar + i)
  })
  pos <- c(list(0), pos, list(nchar))

  lapply(1:(length(pos) - 1), function(i) {
    substr(x, pos[[i]] + 1, pos[[i + 1]])
  })
}

str_split_n <- function(x, pattern, n_max = -1) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  m <- gregexpr(pattern, x, perl = TRUE)
  if (n_max > 0) {
    m <- lapply(m, function(x) slice_match(x, seq_along(x) < n_max))
  }
  regmatches(x, m, invert = TRUE)
}

slice_match <- function(x, i) {
  structure(
    x[i],
    match.length = attr(x, "match.length")[i],
    index.type = attr(x, "index.type"),
    useBytes = attr(x, "useBytes")
  )
}

list_indices <- function(x, max = 20) {
  if (length(x) > max) {
    x <- c(x[seq_len(max)], "...")
  }

  paste(x, collapse = ", ")
}

# pmap()/pmap_chr()  -----------------------------------------------------------------

args_recycle <- function(args) {
  lengths <- vapply(args, length, integer(1))
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- lapply(args[to_recycle], function(x) rep.int(x, n))

  args
}

pmap <- function(.l, .f, ...) {
  args <- args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}

pmap_chr <- function(.l, .f, ...) {
  as.character(pmap(.l, .f, ...))
}
