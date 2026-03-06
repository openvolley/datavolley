# Court zones to x, y coordinates

Generate x and y coordinates for plotting, from DataVolley numbered
zones

## Usage

``` r
ggxy(zones, end="lower", xynames=c("x", "y"), as_for_serve=FALSE)
```

## Arguments

- zones:

  numeric: zones numbers 1-9 to convert to x and y coordinates

- end:

  string: use the "lower" or "upper" part of the figure

- xynames:

  character: names to use for the x and y columns of the returned
  data.frame

- as_for_serve:

  logical: if TRUE, treat positions as for serving. Only zones 1,5,6,7,9
  are meaningful in this case

## Value

data.frame with columns "x" and "y" (or other names if specified in
`xynames`)

## See also

[`datavolley-deprecated`](datavolley-deprecated.md)
