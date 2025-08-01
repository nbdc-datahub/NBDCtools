% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{get_metadata}
\alias{get_metadata}
\title{Get metadata}
\usage{
get_metadata(
  study,
  release = "latest",
  vars = NULL,
  tables = NULL,
  type = "dd"
)
}
\arguments{
\item{study}{character. The study name. One of "abcd" or "hbcd".}

\item{release}{character. Release version (Default: \code{"latest"}).}

\item{vars}{character (vector). Vector with the names of variables to be
included.}

\item{tables}{character (vector). Vector with the names of tables to be
included.}

\item{type}{character. Type of metadata to retrieve. One of \code{"dd"},
\code{"levels"}, \code{"sessions"} (Default: \code{"dd"}).}
}
\value{
Data frame with the metadata.
}
\description{
Retrieves metadata (data dictionary, levels table, event map)
for a given study and release version. Allows for filtering by
variables and tables.
}
\examples{
get_metadata("abcd", type = "levels")

get_metadata("hbcd", release = "1.0")

get_metadata("abcd", vars = c("ab_g_dyn__visit_dtt", "ab_g_dyn__visit_age"))

get_metadata("abcd", tables = "ab_g_dyn")

get_metadata("abcd", tables = "ab_g_dyn")

get_metadata("abcd", type = "sessions")
}
