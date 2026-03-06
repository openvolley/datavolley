# Figure out the phase of play associated with each point

Phase is either "Serve", "Reception" (serve reception and the set and
attack immediately following it, as well as the opposition block on that
attack), or "Transition" (all play actions after that)

## Usage

``` r
play_phase(x, method = "default")
```

## Arguments

- x:

  datavolleyplays: the plays component of a datavolley object as
  returned by `dv_read`

- method:

  string: "default" (uses the `team_touch_id` and `skill` values to
  figure out phase), or "alt" (uses the sequences of `skill` values
  only. This is slower and probably less reliable, but will be more
  likely to give correct results in some situations (e.g. if the
  DataVolley file has been scouted in practice mode, and all actions
  have been assigned to the one team)

## Value

character vector

## See also

[`dv_read`](dv_read.md) [`plays`](plays.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
  px <- plays(x)
  px$phase <- play_phase(px)
} # }
```
