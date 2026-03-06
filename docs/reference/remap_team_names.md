# Change team names

A team name can sometimes be spelled incorrectly, particularly if there
are character encoding issues. This can be a particular problem when
combining data from multiple files. If a team name matches the `from`
entry and/or its ID matches the `team_id` entry in a row in `remap`, the
team will be renamed to the corresponding `to` value and/or its ID
changed to the corresponding `to_team_id` value.

## Usage

``` r
remap_team_names(x, remap, fixed = TRUE)
```

## Arguments

- x:

  datavolley: a datavolley object as returned by `dv_read`, or list of
  such objects

- remap:

  data.frame: data.frame of strings with one or both columns `from` and
  `team_id`, and one or both columns `to` and `to_team_id`

- fixed:

  logical: treat the `from` and `team_id` entries as fixed strings? If
  `fixed` is `FALSE` they will be treated as regular expressions

## Value

datavolley object or list with corresponding team names changed

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
  summary(x)

  ## rename a team based just on team name
  summary(remap_team_names(x, data.frame(from="Nova KBM Branik", to="NKBM Branik")))

  ## rename a team based on team name and ID
  summary(remap_team_names(x, data.frame(from="Nova KBM Branik", to="NKBM Branik", team_id="MB4")))
} # }
```
