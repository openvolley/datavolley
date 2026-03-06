# Attempt to repair a datavolley object

Currently an attempt will be made to repair these issues:

- if multiple players on the same team have the same jersey number,
  players with that number (on that team) who did not take to the court
  will be removed from their team roster. In this situation, whether or
  not a player took to the court is determined from the match metadata
  only

- if multiple players have the same player ID but different jersey
  numbers, players with that ID who did not take to the court will be
  removed from their team roster. In this situation, whether or not a
  player took to the court is determined from the match metadata and the
  play-by-play data

## Usage

``` r
dv_repair(x)
```

## Arguments

- x:

  datavolley: a datavolley object as returned by
  [`dv_read()`](dv_read.md)

## Value

A modified copy of `x`. If problems exist and cannot be repaired, an
error will be thrown
