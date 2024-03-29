% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plays.R
\name{dv_attack_code2side}
\alias{dv_attack_code2side}
\title{Attack side for standard attack codes}
\usage{
dv_attack_code2side(code)
}
\arguments{
\item{code}{character: vector of attack codes ("X5", "VP", etc)}
}
\value{
A named vector of sides ("L", "R", "C")
}
\description{
Attack side for standard attack codes
}
\examples{
dv_attack_code2side(code = c("X5", "X7", "PP"))

}
