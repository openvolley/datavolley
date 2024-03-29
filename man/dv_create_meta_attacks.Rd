% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plays.R
\name{dv_create_meta_attacks}
\alias{dv_create_meta_attacks}
\title{Create a meta attack data.frame from the plays object if it is missing}
\usage{
dv_create_meta_attacks(plays)
}
\arguments{
\item{plays}{data.frame: the plays component of a datavolley object, as returned by \code{\link{dv_read}}}
}
\value{
A data.frame of attacks.
}
\description{
If your DataVolley file does not have a meta attack dataframe (for example, if you are using Click&Scout), this function will create one from the information in the plays object.
}
