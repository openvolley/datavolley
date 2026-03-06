# Find each entry in y that follows each entry in x

Find each entry in y that follows each entry in x

## Usage

``` r
findnext(x, y)
```

## Arguments

- x:

  numeric: vector

- y:

  numeric: vector

## Value

vector, each entry is the value in y that is next-largest to each
corresponding entry in x

## Examples

``` r
findnext(c(1, 5, 10), c(1, 2, 3, 7, 8, 9))
#> [1]  2  7 NA
```
