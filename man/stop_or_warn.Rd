% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrdata.R
\name{stop_or_warn}
\alias{stop_or_warn}
\title{Stop or show warning}
\usage{
stop_or_warn(msg)
}
\arguments{
\item{msg}{Message to display.}
}
\value{
None.
}
\description{
Internal helper for \code{metrdata}, either stop with an error or show warning
(if strict mode is \code{FALSE}).
}
\examples{
testthat::expect_error(metrdata:::stop_or_warn("problem encountered"))
}
