% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrdata.R
\name{read_with_metadata}
\alias{read_with_metadata}
\alias{read_tsv_metadata}
\alias{read_csv_metadata}
\alias{read_csv2_metadata}
\alias{read_delim_metadata}
\title{Read a file together with its metadata}
\usage{
read_with_metadata(path, read_func, ..., metadata_args = list())

read_tsv_metadata(path, ..., metadata_args = list())

read_csv_metadata(path, ..., metadata_args = list())

read_csv2_metadata(path, ..., metadata_args = list())

read_delim_metadata(path, ..., metadata_args = list())
}
\arguments{
\item{path}{Path to read data from. The path for the metadata sidecar file
is automatically derived by appending the expected suffix to this path.}

\item{read_func}{For \code{read_with_metadata}, the user should provide the
function used for reading the data from \code{path}.}

\item{...}{Additional parameters passed to the read function.}

\item{metadata_args}{Additional parameters passed to \code{read_metadata()}.}
}
\value{
The data object read using the read function, with metadata read
from the sidecar file attached as an attribute.
}
\description{
This series of functions read a file together with its associated metadata.
The metadata is stored as a \code{metrdata} object in the \code{metrdata}
attributed for the resulting data object.
}
\details{
\code{read_with_metadata()} allows a data file to be read with a user-provided
function for reading from \code{path}.

\code{read_tsv_metadata()} reads the data file from \code{path} using \code{readr::read_tsv()}.

\code{read_csv_metadata()} reads the data file from \code{path} using \code{readr::read_csv()}.

\code{read_csv2_metadata()} reads the data file from \code{path} using \code{readr::read_csv2()}.

\code{read_delim_metadata()} reads the data file from \code{path} using \code{readr::read_delim()}.
}
\seealso{
\code{\link[=read_metadata]{read_metadata()}} \code{\link[readr:read_delim]{readr::read_tsv()}} \code{\link[readr:read_delim]{readr::read_csv()}}
\code{\link[readr:read_delim]{readr::read_csv2()}} \code{\link[readr:read_delim]{readr::read_delim()}}

a_data_frame <- data.frame(index = 1:10)
metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
tmpfilename<-tempfile()
write_with_metadata(a_data_frame, tmpfilename, write_func = write.csv, row.names = FALSE)
a_data_frame2<-read_with_metadata(tmpfilename, read_func = read.csv)
testthat::expect_equal(a_data_frame, a_data_frame2)

write_tsv_metadata(a_data_frame, tmpfilename)
a_data_frame2<-read_tsv_metadata(tmpfilename) # reread as tibble, so ignore class
testthat::expect_equal(a_data_frame, a_data_frame2, ignore_attr = TRUE)
testthat::expect_equal(metadata(a_data_frame), metadata(a_data_frame2))

write_csv_metadata(a_data_frame, tmpfilename)
a_data_frame3<-read_csv_metadata(tmpfilename)
testthat::expect_equal(a_data_frame3, a_data_frame2)

write_csv2_metadata(a_data_frame, tmpfilename)
a_data_frame4<-read_csv2_metadata(tmpfilename)
testthat::expect_equal(a_data_frame4, a_data_frame2)

write_delim_metadata(a_data_frame, tmpfilename, delim=",")
a_data_frame5<-read_delim_metadata(tmpfilename, delim=",")
testthat::expect_equal(a_data_frame5, a_data_frame2)

file.remove(tmpfilename)
file.remove(metrdata:::append_metrdata_suffix(tmpfilename))
}
