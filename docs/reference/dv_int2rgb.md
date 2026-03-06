# Convert integer colour to RGB

DataVolley files use an integer representation of colours. These
functions convert to and from hex colour strings as used in R.

## Usage

``` r
dv_int2rgb(z)

dv_rgb2int(x)
```

## Arguments

- z:

  integer: vector of integers

- x:

  integer: vector of hex colour strings

## Value

Character vector of hex RGB colour strings

## Examples

``` r
dv_int2rgb(c(255, 16711680))
#> [1] "#0000FF" "#FF0000"
```
