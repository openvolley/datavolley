# A simple summary of a volleyball match

A simple summary of a volleyball match

## Usage

``` r
# S3 method for class 'datavolley'
summary(object, ...)
```

## Arguments

- object:

  datavolley: datavolley object as returned by `dv_read`

- ...:

  : additional arguments (currently these have no effect)

## Value

list of summary items

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
summary(x)
#> Match summary:
#> Date: 2015-01-25
#> League: Finale mladinke
#> Teams: Braslovče (JERONČIČ ZORAN/MIHALINEC DAMIJANA)
#>        vs
#>        Nova KBM Branik (HAFNER MATJAŽ)
#> Result: 3-0 (25-16, 25-14, 25-22)
#> Duration: 67 minutes
```
