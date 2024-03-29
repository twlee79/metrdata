% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrdata.R
\name{set_metadata}
\alias{set_metadata}
\alias{metadata<-}
\title{Assign metadata of an object}
\usage{
set_metadata(obj, value, replace = FALSE)

metadata(obj) <- value
}
\arguments{
\item{obj}{Object to assign metadata to; should not already have metadata.}

\item{value}{\code{metrdata} object to assign as metadata.}

\item{replace}{If \code{TRUE}, existing metadata in obj may be replaced; defaults
to error if attempting to replace metadata.}
}
\value{
\code{obj} with metadata.
}
\description{
Directly assign the metadata of an object. The object must not already have
metadata and the provided value must be a \code{metrdata} object.
}
\details{
The \code{metrdata} object can be constructed using the \code{metrdata()} constructor,
or using \code{prepare_metadata(label, metadata)}.
}
\examples{
a_data_frame <- data.frame(index = 1:10)
metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
get_metadata(a_data_frame)

a_data_frame2 <- data.frame(index = 1:20)
a_data_frame2 <- set_metadata(a_data_frame2, metrdata(list(note = "Indexes 1 to 20")))
get_metadata(a_data_frame2)
}
