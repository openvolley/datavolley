# Attempt to build a player name remapping table

A player name can sometimes be spelled incorrectly, particularly if
there are character encoding issues. This can be a particular problem
when combining data from multiple files. This function will attempt to
find names that have been misspelled and create a remapping table
suitable to pass to [`remap_player_names`](remap_player_names.md).
Player names will only be compared within the same team. Note that this
function is unlikely to get perfect results: use its output with care.

## Usage

``` r
find_player_name_remapping(x, distance_threshold = 3, verbose = TRUE)
```

## Arguments

- x:

  datavolley: a datavolley object as returned by `dv_read`, or list of
  such objects

- distance_threshold:

  numeric: if two names differ by an amount less than this threshold,
  they will be treated as the same name

- verbose:

  logical: print progress to console as we go? Note that warnings will
  also be issued regardless of this setting

## Value

data.frame with columns team, from, to

## See also

[`remap_player_names`](remap_player_names.md),
[`check_player_names`](check_player_names.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
  remap <- find_player_name_remapping(x)
} # }
```
