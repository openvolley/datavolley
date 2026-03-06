# Point phase

Point phase as defined by DataVolley: either "Sideout" or "Breakpoint",
assigned only to winning or losing actions (including green codes). Note
that the point phase is inferred for the winning action (i.e. the point
phase value for both the winning and losing action is "Sideout" if the
winning team was receiving).

## Usage

``` r
dv_point_phase(x)
```

## Arguments

- x:

  datavolleyplays: the plays component of a datavolley object as
  returned by [`dv_read()`](dv_read.md)

## Value

Character vector
