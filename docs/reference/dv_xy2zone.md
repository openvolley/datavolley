# Convert x, y coordinates to zones

Convert x, y coordinates to zones

## Usage

``` r
dv_xy2zone(x, y = NULL, as_for_serve = FALSE)
```

## Arguments

- x:

  numeric: the x coordinate

- y:

  numeric: the y coordinate. If `y` is `NULL`, `x` will be treated as a
  grid index (see [`dv_index2xy`](dv_index2xy.md))

- as_for_serve:

  logical: if `TRUE`, treat the zones as if they refer to serving
  locations (i.e. zone 7 in between zones 5 and 6, and zone 9 in between
  zones 6 and 1)

## Value

A numeric vector giving the zone number

## See also

[`dv_xy2index`](dv_index2xy.md), [`dv_index2xy`](dv_index2xy.md),
[`dv_cone2xy`](dv_cone2xy.md), [`dv_xy2subzone`](dv_xy2subzone.md)

## Examples

``` r
if (FALSE) { # \dontrun{

## a bunch of random points on and around the court
idx <- round(runif(100, min = 1, max = 10000))

## convert to zones
zn <- dv_xy2zone(x = idx)

## or, equivalently, convert the index to xy values first
idx_xy <- dv_index2xy(idx)
zn <- dv_xy2zone(x = idx_xy$x, idx_xy$y)

## plot
ggplot(idx_xy, aes(x, y, fill = as.factor(zn))) + geom_point(shape = 21) +
  ggcourt(labels = NULL)

## the points shoud be coloured by zone
} # }
```
