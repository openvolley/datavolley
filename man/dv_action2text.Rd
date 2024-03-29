% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{dv_action2text}
\alias{dv_action2text}
\title{Generate a short, human-readable text summary of one or more actions}
\usage{
dv_action2text(x, verbosity = 1)
}
\arguments{
\item{x}{data.frame or tibble: one or more rows from a datavolleyplays object as returned by \code{\link{dv_read}}}

\item{verbosity}{integer: 1 = least verbose, 2 = more verbose. Currently ignored}
}
\value{
character vector
}
\description{
Generate a short, human-readable text summary of one or more actions
}
\examples{
x <- dv_read(dv_example_file())
dv_action2text(plays(x)[27:30, ])

}
