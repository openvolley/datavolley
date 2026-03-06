# Change player names

A player name can sometimes be spelled incorrectly, particularly if
there are character encoding issues. This can be a particular problem
when combining data from multiple files. A player matching the `team`
and `from` name entries in a row in `remap` is renamed to the
corresponding `to` value. Alternatively, `remap` can be provided with
the columns `player_id` and `player_name`: all player name entries
associated with a given `player_id` will be changed to the associated
`player_name`.

## Usage

``` r
remap_player_names(x, remap)
```

## Arguments

- x:

  datavolley: a datavolley object as returned by `dv_read`, or list of
  such objects

- remap:

  data.frame: data.frame of strings with columns team, from, and to

## Value

A datavolley object or list with corresponding player names changed

## See also

[`dv_read`](dv_read.md), [`check_player_names`](check_player_names.md),
[`find_player_name_remapping`](find_player_name_remapping.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
  x <- remap_player_names(x, data.frame(team = c("Nova KBM Branik", "Braslovče"),
                                        from = c("ELA PINTAR", "KATJA MIHALINEC"),
                                        to = c("Ela PINTAR", "Katja MIHALINEC"),
                                        stringsAsFactors = FALSE))

  x <- remap_player_names(x, data.frame(player_id = c("id1", "id2"),
                                        player_name = c("name to use 1", "name to use 2"),
                                        stringsAsFactors = FALSE))
} # }
```
