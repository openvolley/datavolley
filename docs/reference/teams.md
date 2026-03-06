# Get team names and IDs from datavolley object

Get team names and IDs from datavolley object

## Usage

``` r
teams(x)

home_team(x)

home_team_id(x)

visiting_team(x)

visiting_team_id(x)
```

## Arguments

- x:

  datavolley or data.frame: a datavolley object as returned by
  `dv_read`, or the plays component of that object

## Value

character vector of team names or IDs

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
  teams(x)
  home_team_id(x)
} # }
```
