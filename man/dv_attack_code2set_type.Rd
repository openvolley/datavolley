% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plays.R
\name{dv_attack_code2set_type}
\alias{dv_attack_code2set_type}
\title{Set type for standard attack codes}
\usage{
dv_attack_code2set_type(code)
}
\arguments{
\item{code}{character: vector of attack codes ("X5", "VP", etc)}
}
\value{
A named vector of sides ("F", "B", "C", "P", "S", "-")
}
\description{
Set type for standard attack codes
}
\examples{
dv_attack_code2set_type(code = c("X5", "X7", "PP"))

}
