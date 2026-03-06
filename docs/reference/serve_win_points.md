# Find serve win points

Find points in which the serving team wins the point. Serve win rate is
the fraction of serves won by the serving team.

## Usage

``` r
serve_win_points(x, return_id = FALSE)
```

## Arguments

- x:

  data.frame: the plays component of a datavolley object, as returned by
  [`dv_read()`](dv_read.md)

- return_id:

  logical: include the match_id and point_id of all serve win points in
  the returned object?

## Value

named list with components "ix" (logical indices of serves corresponding
to serve win points in the x object), "n" (number of serve win points in
x), "rate" (serve win rate from x). If `return_id` is TRUE, also return
a component "id" (a data.frame containing the match_id and point_id of
all serve win points)

## See also

[`dv_read`](dv_read.md) [`plays`](plays.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
serve_idx <- find_serves(plays(x))
swp <- serve_win_points(plays(x))
## number of serves by team
table(plays(x)$team[serve_idx])
## number of points won on serve by team
table(plays(x)$team[serve_idx & swp$ix])
} # }
```
