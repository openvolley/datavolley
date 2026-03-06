# Find coordinates that need flipping

The orientation of coordinates (e.g. is a serve going from the lower
part of the court to the upper, or vice-versa?) depends on how the scout
entered them. This function finds coordinates that require flipping, so
that all attacks/serves/whatever can be plotted with the same
orientation

## Usage

``` r
dv_find_to_flip_coordinates(x, target_start_end = "lower")
```

## Arguments

- x:

  datavolleyplays: the plays component of a datavolley object as
  returned by `dv_read`

- target_start_end:

  string: "lower" or "upper"

## Value

A logical index with length equal to the number of rows of `x`. TRUE
indicates rows of `x` that need their coordinates flipped

## See also

[`dv_flip_xy`](dv_flip_xy.md)
