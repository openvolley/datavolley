# Grid index to x,y coordinate and vice-versa

DataVolley uses a grid to represent positions on court (values in
columns "start_coordinate", "mid_coordinate", and "end_coordinate" in
the play-by-play data frame). These functions convert grid index values
to x, y coordinates suitable for plotting, and vice-versa. For a
description of the court dimensons and coordinates see
[`ggcourt`](ggcourt.md).

## Usage

``` r
dv_index2xy(index)

dv_xy2index(x, y)
```

## Arguments

- index:

  integer: vector of grid indices. If missing, the entire grid will be
  returned. The row numbers match the grid indices

- x:

  numeric: x-coordinate. For `dv_index2xy` this can be a two-column
  matrix or data.frame containing x and y

- y:

  numeric: y-coordinate

## Value

for dv_index2xy, a data.frame with columns "x" and "y"; for dv_xy2index
a vector of integer values

## See also

[`ggcourt`](ggcourt.md), [`dv_xy`](dv_xy.md),
[`dv_flip_xy`](dv_flip_xy.md), [`dv_xy2zone`](dv_xy2zone.md),
[`dv_xy2subzone`](dv_xy2subzone.md)

## Examples

``` r
## positions (zones) 1 and 3 are at x, y coordinates c(3, 1) and c(2, 3) respectively

## their grid indices:
dv_xy2index(c(3, 2), c(1, 3))
#> [1] 1677 4351
```
