% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{dv_xy2cone}
\alias{dv_xy2cone}
\title{Convert x, y coordinates to cones}
\usage{
dv_xy2cone(x, y = NULL, start_zones, force_center_zone = FALSE)
}
\arguments{
\item{x}{numeric: the x coordinate}

\item{y}{numeric: the y coordinate. If \code{y} is \code{NULL}, \code{x} will be treated as a grid index (see \code{\link{dv_index2xy}})}

\item{start_zones}{numeric or character: the starting zone of each row (values 1-9, or "L", "M", "R")}

\item{force_center_zone}{logical: a vector indicating the rows that should be treated as center zone attacks regardless of their start_zone value (e.g. attacks by the setter). If \code{FALSE}, the start_zone value will be used. If provided as a single scalar value, this will be applied to all attacks}
}
\value{
A numeric vector giving the cone number
}
\description{
Convert x, y coordinates to cones
}
\examples{
\dontrun{

## a bunch of random points on and around the court
idx <- round(runif(100, min = 1, max = 10000))

## convert to cones, assuming a start_zone of "L"
cn <- dv_xy2cone(x = idx, start_zones = "M")

## generate the cone polygons for reference
cxy <- dv_cone_polygons("M")
cxyl <- dv_cone_polygons("M", end = "lower")

## plot
ggplot(cxy, aes(x, y, group = cone_number, fill = as.factor(cone_number))) +
  ## the cone polygons
  geom_polygon() + geom_polygon(data = cxyl) +
  ggcourt(labels = NULL) +
  ## and our points
  geom_point(data = dv_index2xy(idx) \%>\% mutate(cone_number = cn), shape = 21,
             colour = "black", size = 2)

## the points shoud be coloured the same as the cone polygons
}

}
\seealso{
\code{\link{dv_xy2index}}, \code{\link{dv_index2xy}}, \code{\link{dv_cone2xy}}, \code{\link{dv_xy2zone}}, \code{\link{dv_xy2subzone}}
}
