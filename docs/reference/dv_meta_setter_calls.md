# Extract the setter calls table from a datavolley object

Extract the setter calls table from a datavolley object

## Usage

``` r
dv_meta_setter_calls(x)
```

## Arguments

- x:

  datavolley: as returned by [`dv_read()`](dv_read.md)

## Value

The setter calls table from the metadata section of `x`

## Details

Note that some columns are placeholders (named e.g. `X2`, `X4` and
generally filled with NAs). These are kept for compatibility reasons.

## Examples

``` r
x <- dv_read(dv_example_file())

## the setter calls in this file
dv_meta_setter_calls(x)
#> # A tibble: 7 × 11
#>   code  X2    description    X4    colour  start_coordinate mid_coordinate
#>   <chr> <lgl> <chr>          <lgl> <chr>              <int>          <int>
#> 1 K1    NA    Front Quick    NA    #FF0000             3949           4454
#> 2 K2    NA    Back Quick     NA    #FF0000             3864           4278
#> 3 K7    NA    Seven          NA    #FF0000             3923           4426
#> 4 KC    NA    Quick in 3     NA    #FF0000             3849           4449
#> 5 KM    NA    shifted to 2   NA    #FF0000                0              0
#> 6 KP    NA    Shifted to 4   NA    #FF0000                0              0
#> 7 KE    NA    No First Tempo NA    #000000                0              0
#> # ℹ 4 more variables: end_coordinate <int>, path <chr>, path_colour <chr>,
#> #   X11 <lgl>
```
