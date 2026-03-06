# Find aces that might not be marked as such

Some DataVolley files do not indicate serve aces with the skill
evaluation "Ace". This function will search for winning serves, either
with no reception or a reception error, and change their evaluation
value to "Ace"

## Usage

``` r
fix_ace_evaluations(x, rotation_error_is_ace = FALSE, verbose = TRUE)
```

## Arguments

- x:

  datavolley: a datavolley object as returned by `dv_read`, or list of
  such objects

- rotation_error_is_ace:

  logical: should a rotation error on reception by the receiving team be
  counted as an ace?

- verbose:

  logical: print progress to console?

## Value

datavolley object or list of such with updated evaluation values

## See also

[`dv_read`](dv_read.md)
