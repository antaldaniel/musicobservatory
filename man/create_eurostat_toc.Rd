% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_eurostat_toc.R
\name{create_eurostat_toc}
\alias{create_eurostat_toc}
\title{Create table of contents for Eurostat data tables

A wrapper around \code{\link[eurostat]{get_eurostat_toc}}}
\usage{
create_eurostat_toc(codes)
}
\arguments{
\item{codes}{Eurostat product codes.}
}
\value{
Table of contents and metadata about indicator updates
in a data frame.
}
\description{
Create table of contents for Eurostat data tables

A wrapper around \code{\link[eurostat]{get_eurostat_toc}}
}
\seealso{
Other Eurostat functions: 
\code{\link{create_eurostat_metadata_table}()},
\code{\link{create_eurostat_tables}()},
\code{\link{create_missings}()},
\code{\link{process_eurostat_indicator}()}
}
\concept{Eurostat functions}
