# Extract the plays component from a datavolley object, or assign a new one

Extract the plays component from a datavolley object, or assign a new
one

## Usage

``` r
plays(x)

plays(x) <- value
```

## Arguments

- x:

  datavolley: a datavolley object as returned by `dv_read`

- value:

  datavolleyplays: new data

## Value

The plays component of x (a data.frame), or a modified version of x with
the new plays component inserted

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
  inspect(plays(x))

  p2 <- plays(x)
  plays(x) <- p2
} # }
```
