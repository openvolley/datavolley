# Check for similar player names

Player names can sometimes be spelled incorrectly, particularly if there
are character encoding issues. This can be a particular problem when
combining data from multiple files. This function checks for similar
names that might possibly be multiple variants on the same name.

## Usage

``` r
check_player_names(x, distance_threshold = 4)
```

## Arguments

- x:

  datavolley: a datavolley object as returned by `dv_read`, or list of
  such objects

- distance_threshold:

  numeric: if two names differ by an amount less than this threshold,
  they will be returned as possible matches

## Value

data.frame

## See also

[`dv_read`](dv_read.md), [`adist`](https://rdrr.io/r/utils/adist.html)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
  check_player_names(x)
} # }
```
