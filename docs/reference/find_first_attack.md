# Find first attacks by the receiving team (i.e. attacks associated with a serve reception)

Find first attacks by the receiving team (i.e. attacks associated with a
serve reception)

## Usage

``` r
find_first_attack(x)
```

## Arguments

- x:

  data.frame: the plays component of a datavolley object, as returned by
  [`dv_read()`](dv_read.md)

## Value

named list with components "ix" (logical indices into the x object where
the row corresponds to a first attack in a rally), "n" (number of
receptions for which there was a first attack by the receiving team),
"n_win" (the number of winning first attacks), "win_rate" (number of
winning first attacks as a proportion of the total number of first
attacks).

## See also

[`dv_read`](dv_read.md) [`plays`](plays.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
## first attack win rate, by team
by(plays(x),plays(x)$team,function(z)find_first_attack(z)$win_rate)
} # }
```
