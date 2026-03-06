# Attack cones to x, y coordinates

Attack cones to x, y coordinates

## Usage

``` r
dv_cone2xy(
  start_zones,
  end_cones,
  end = "upper",
  xynames = c("ex", "ey"),
  as = "points",
  force_center_zone = FALSE
)
```

## Arguments

- start_zones:

  integer: starting zone of attack

- end_cones:

  integer: cone of attack

- end:

  string: use the "lower" or "upper" part of the figure

- xynames:

  character: names to use for the x and y columns of the returned
  data.frame

- as:

  string: either "points" or "polygons" (see Value, below)

- force_center_zone:

  logical: a vector indicating the attacks that should be treated as
  center zone attacks regardless of their start_zone value (e.g. by the
  setter). If `FALSE`, the start_zone value will be used. If provided as
  a single scalar value, this will be applied to all attacks

## Value

a tibble (NOT a data.frame) with columns "x" and "y" (or other names if
specified in `xynames`). If `as` is "polygons", the columns will be
lists, because each polygon will have four x- and y-coordinates

## See also

[`ggcourt`](ggcourt.md), [`dv_flip_xy`](dv_flip_xy.md),
[`dv_xy2index`](dv_index2xy.md), [`dv_index2xy`](dv_index2xy.md),
[`dv_xy`](dv_xy.md), [`dv_xy2zone`](dv_xy2zone.md),
[`dv_xy2subzone`](dv_xy2subzone.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## attacks from left side (zone 4) to cones 1-7

## plot as line segments
cxy <- dv_cone2xy(4, 1:7)
## add starting coordinate for zone 4
cxy <- cbind(dv_xy(4), cxy)
ggplot(cxy, aes(x, y, xend=ex, yend=ey)) + geom_segment() + ggcourt()

## plot as polygons
cxy <- dv_cone2xy(4, 1:7, as = "polygons")

## this returns coordinates as list columns, unpack these to use with ggplot
##  also add an identifier for each polygon
cxy <- data.frame(x = unlist(cxy$ex), y = unlist(cxy$ey),
                  id = unlist(lapply(1:nrow(cxy), rep, 4)))
ggplot(cxy, aes(x, y, group = id, fill = as.factor(id))) + geom_polygon() +
   ggcourt()
} # }
```
