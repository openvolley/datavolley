# Find each entry in y that precedes each entry in x

Find each entry in y that precedes each entry in x

## Usage

``` r
findprev(x, y)
```

## Arguments

- x:

  numeric: vector

- y:

  numeric: vector

## Value

vector, each entry is the value in y that is next-smallest to each
corresponding entry in x

## Examples

``` r
findprev(c(1, 5, 10), c(1, 2, 3, 7, 8, 9))
#> [1] NA  3  9
```
