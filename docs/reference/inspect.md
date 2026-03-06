# Convenience function for inspecting the plays component of a datavolley object

Convenience function for inspecting the plays component of a datavolley
object

## Usage

``` r
inspect(x, vars = "minimal", maxrows = 100, extra)
```

## Arguments

- x:

  datavolleyplays: the plays component of a datavolley object as
  returned by `dv_read`

- vars:

  string: which variables to print? "minimal" set or "all"

- maxrows:

  numeric: maximum number of rows to print

- extra:

  character: names of any extra columns to include in the output

## See also

[`dv_read`](dv_read.md) [`plays`](plays.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
  inspect(plays(x))
} # }
```
