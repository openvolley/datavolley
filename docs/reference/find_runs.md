# Generate information about runs of events

Find runs of events within a match. Typically, this function would be
passed a subset of `plays(x)`, such as rows corresponding to serves.
Runs that are terminated by the end of a set are not assigned a
`run_length`.

## Usage

``` r
find_runs(x, idvars = "team", within_set = TRUE)
```

## Arguments

- x:

  data.frame: a subset of the plays component of a datavolley object, as
  returned by [`dv_read()`](dv_read.md)

- idvars:

  character: string or character vector of variabe names to use to
  identify the entity doing the events

- within_set:

  logical: only consider runs within a single set? If FALSE, runs that
  span sets will be treated as a single run

## Value

A data.frame the same number of rows as `x`, and with columns `run_id`
(the identifier of the run to which each row belongs), `run_length` (the
length of the run), and `run_position` (the position of this row in its
associated run).

## See also

[`dv_read`](dv_read.md) [`plays`](plays.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## find runs of serves
x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
serve_idx <- find_serves(plays(x))
serve_run_info <- find_runs(plays(x)[serve_idx,])
## distribution of serve run lengths
table(unique(serve_run_info[,c("run_id","run_length")])$run_length)
} # }
```
