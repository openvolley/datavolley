# Find serves

Find serves

## Usage

``` r
find_serves(x)
```

## Arguments

- x:

  data.frame: the plays component of a datavolley object, as returned by
  [`dv_read()`](dv_read.md)

## Value

a logical vector, giving the indices of the rows of x that correspond to
serves

## See also

[`dv_read`](dv_read.md) [`plays`](plays.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
serve_idx <- find_serves(plays(x))
## number of serves by team
table(plays(x)$team[serve_idx])
} # }
```
