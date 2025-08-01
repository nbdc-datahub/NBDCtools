% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{benchmark_models}
\alias{benchmark_models}
\title{Benchmark Models}
\format{
A list of benchmark models to estimate time and memory usage
for loading data.
\itemize{
\item time_small: A model for estimating time for small datasets (n_var < 1000).
\item time_large: A model for estimating time for
larger datasets (n_var >= 1000).
\item ram: A model for estimating RAM usage based on the number of variables.
}

\strong{Internal use only}: This dataset is used internally by some functions
and used in the package vignettes.
It is not intended for direct use by the end user.
}
\usage{
benchmark_models
}
\description{
Benchmark Models
}
\keyword{data}
\keyword{internal}
