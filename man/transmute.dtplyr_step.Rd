% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step-subset-transmute.R
\name{transmute.dtplyr_step}
\alias{transmute.dtplyr_step}
\title{Create new columns, dropping old}
\usage{
\method{transmute}{dtplyr_step}(.data, ...)
}
\arguments{
\item{.data}{A \code{\link[=lazy_dt]{lazy_dt()}}.}

\item{...}{<\code{\link[rlang:args_data_masking]{data-masking}}> Name-value pairs.
The name gives the name of the column in the output.

The value can be:
\itemize{
\item A vector of length 1, which will be recycled to the correct length.
\item A vector the same length as the current group (or the whole data frame
if ungrouped).
\item \code{NULL}, to remove the column.
\item A data frame or tibble, to create multiple columns in the output.
}}
}
\description{
This is a method for the dplyr \code{\link[=transmute]{transmute()}} generic. It is translated to
the \code{j} argument of \verb{[.data.table}.
}
\examples{
library(dplyr, warn.conflicts = FALSE)

dt <- lazy_dt(dplyr::starwars)
dt \%>\% transmute(name, sh = paste0(species, "/", homeworld))
}
