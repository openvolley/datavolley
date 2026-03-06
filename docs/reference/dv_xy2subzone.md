# Convert x, y coordinates to zones and subzones

Convert x, y coordinates to zones and subzones

## Usage

``` r
dv_xy2subzone(x, y = NULL)
```

## Arguments

- x:

  numeric: the x coordinate

- y:

  numeric: the y coordinate. If `y` is `NULL`, `x` will be treated as a
  grid index (see [`dv_index2xy`](dv_index2xy.md))

## Value

A tibble with columns `zone` and `subzone`

## See also

[`dv_xy2index`](dv_index2xy.md), [`dv_index2xy`](dv_index2xy.md),
[`dv_cone2xy`](dv_cone2xy.md), [`dv_xy2zone`](dv_xy2zone.md)

## Examples

``` r
if (FALSE) { # \dontrun{

## a bunch of random points on and around the court
idx <- round(runif(100, min = 1, max = 10000))

## convert to zones
zn <- dv_xy2subzone(x = idx)

## or, equivalently, convert the index to xy values first
zn <- cbind(zn, dv_index2xy(idx))

## plot
ggplot(zn, aes(x, y, colour = as.factor(zone), shape = subzone)) + geom_point(size = 3) +
  ggcourt(labels = NULL)

## the points shoud be coloured by zone
} # }
```
