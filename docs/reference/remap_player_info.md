# Change player information

An experimental function to replace `remap_player_names` as a more
comprehensive remapping of player attributes.

## Usage

``` r
remap_player_info(x, remap)
```

## Arguments

- x:

  datavolley: a datavolley object as returned by `dv_read`, or list of
  such objects

- remap:

  data.frame: data.frame of strings with columns team, name_from, and
  any of player_id, firstname, and lastname

## Value

A datavolley object or list with corresponding player names changed

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
  x <- remap_player_info(x, data.frame(team = c("Nova KBM Branik", "Braslovče"),
                                       name_from = c("ELA PINTAR", "KATJA MIHALINEC"),
                                       firstname = c("Ela", "Katja"), stringsAsFactors = FALSE))
} # }
```
