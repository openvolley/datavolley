# Summarize a list of volleyball matches

Summarize a list of volleyball matches

## Usage

``` r
dvlist_summary(z)
```

## Arguments

- z:

  list: list of datavolley objects as returned by `dv_read`

## Value

named list with various summary indicators, including a competition
ladder

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
  dvlist_summary(list(x,x)) ## same match duplicated twice, just for illustration purposes
} # }
```
