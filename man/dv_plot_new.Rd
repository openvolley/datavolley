% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_base.R
\name{dv_plot_new}
\alias{dv_plot_new}
\title{Create a new plot page for base graphics plotting}
\usage{
dv_plot_new(x, y, legend, court, margins, par_args, ...)
}
\arguments{
\item{x}{numeric or RasterLayer: x-coordinates of the data to plot, or a \code{RasterLayer} layer defining the extent of the data}

\item{y}{numeric: y-coordinates of the data to plot. Not needed if \code{x} is a \code{RasterLayer} object}

\item{legend}{logical: if \code{TRUE}, leave space for a legend}

\item{court}{string: either "full", "lower", or "upper"}

\item{margins}{numeric: vector of four values to use as margins (bottom, left, top, right). Values are as a proportion of the plot size}

\item{par_args}{list: parameters to pass to \code{\link{par}}}

\item{...}{: additional parameters passed to \code{\link{plot.window}}}
}
\description{
The plot will be set up as either a full- or half-court plot, depending on the inputs. The extent can be specified via the \code{court} argument (values either "full", "lower", or "upper"), or via the \code{x} and \code{y} arguments. If the latter, provide either separate \code{x} and \code{y} numeric vectors, or as a single \code{x} \code{RasterLayer} object. If no extent is specified by any of these methods, a full-court plot is assumed.
}
\examples{
dv_plot_new()
## show an attack from position 4 to position 6
from <- dv_xy(4, end = "lower")
to <- dv_xy(6, end = "upper")
lines(c(from[1], to[1]), c(from[2], to[2]), col = "green")
## add the court diagram
dv_court(labels = c("Attacking team", "Defending team"))

}
\seealso{
\code{\link{dv_court}}, \code{\link{dv_heatmap}}
}
