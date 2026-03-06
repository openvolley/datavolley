# The polygon coordinates for attack cones

The polygon coordinates for attack cones

## Usage

``` r
dv_cone_polygons(zone, end = "upper", extended = FALSE)
```

## Arguments

- zone:

  string: one of "L", "R", "M"

- end:

  string: use the "lower" or "upper" part of the figure

- extended:

  logical: if `FALSE`, the polygons will only cover the actual court
  area; if `TRUE`, they will be extended to cover the court periphery as
  well

## Value

A data.frame with columns `cone_number`, `x`, `y`

## Examples

``` r
if (FALSE) { # \dontrun{
 library(ggplot2)
 cxy <- dv_cone_polygons("M")
 ggplot(cxy, aes(x, y, group = cone_number, fill = as.factor(cone_number))) +
   geom_polygon() + ggcourt()
} # }
```
