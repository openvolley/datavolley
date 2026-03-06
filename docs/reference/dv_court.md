# Plot a volleyball court diagram

Volleyball court schematic suitable for adding to a figure

## Usage

``` r
dv_court(
  plot_package = "base",
  court = "full",
  show_zones = TRUE,
  labels = c("Serving team", "Receiving team"),
  as_for_serve = FALSE,
  show_zone_lines = TRUE,
  show_minor_zones = FALSE,
  grid_colour = "black",
  zone_colour = "grey70",
  minor_zone_colour = "grey80",
  fixed_aspect_ratio = TRUE,
  zone_font_size = 10,
  ...
)
```

## Arguments

- plot_package:

  string: either "base" or "ggplot2". If "ggplot2", the
  [`ggcourt`](ggcourt.md) function is used

- court:

  string: "full" (show full court) or "lower" or "upper" (show only the
  lower or upper half of the court)

- show_zones:

  logical: add numbers indicating the court zones (3m squares)?

- labels:

  string: labels for the lower and upper court halves (pass NULL for no
  labels)

- as_for_serve:

  logical: if TRUE and `show_zones` is TRUE, show zones as for serving.
  Only zones 1,5,6,7,9 are meaningful in this case

- show_zone_lines:

  logical: if FALSE, just show the 3m line. If TRUE, also show the 3m x
  3m zones

- show_minor_zones:

  logical: add lines for the subzones (1.5m squares)?

- grid_colour:

  string: colour to use for court sidelines, 3m line, and net

- zone_colour:

  string: colour to use for zone lines and labels

- minor_zone_colour:

  string: colour to use for minor zone grid lines

- fixed_aspect_ratio:

  logical: if TRUE, coerce the plotted court to be square (for a
  half-court plot) or a 2:1 rectangle (full court plot). Prior to
  package version 0.5.3 this was not TRUE by default

- zone_font_size:

  numeric: the font size of the zone labels

- ...:

  : additional parameters passed to `ggplot2::theme_classic(...)`

## Details

The datavolley package uses the following dimensions and coordinates for
plotting:

- the court is shown such that the sidelines are oriented vertically and
  the net is oriented horizontally

- the intersection of the left-hand sideline and the bottom baseline is
  at (0.5, 0.5)

- the intersection of the right-hand sideline and the top baseline is at
  (3.5, 6.5)

- the net intersects the sidelines at (0.5, 3.5) and (3.5, 3.5)

- the zones 1-9 (as defined in the DataVolley manual) on the lower half
  of the court are located at:

  1.  (3, 1)

  2.  (3, 3)

  3.  (2, 3)

  4.  (1, 3)

  5.  (1, 1)

  6.  (2, 1)

  7.  (1, 2)

  8.  (2, 2)

  9.  (3, 2)

- the zones 1-9 (as defined in the DataVolley manual) on the upper half
  of the court are located at:

  1.  (1, 6)

  2.  (1, 4)

  3.  (2, 4)

  4.  (3, 4)

  5.  (3, 6)

  6.  (2, 6)

  7.  (3, 5)

  8.  (2, 5)

  9.  (1, 5)

To get a visual depiction of this, try:
` ggplot() + ggcourt() + theme_bw()`

## See also

[`ggcourt`](ggcourt.md) for a `ggplot2` equivalent function;
[`dv_xy`](dv_xy.md), [`dv_xy2index`](dv_index2xy.md),
[`dv_index2xy`](dv_index2xy.md), [`dv_flip_xy`](dv_flip_xy.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)

library(dplyr)

## Example: attack frequency by zone, per team

attack_rate <- plays(x) %>% dplyr::filter(skill == "Attack") %>%
  group_by(team, start_zone) %>% dplyr::summarize(n_attacks = n()) %>%
  mutate(rate = n_attacks/sum(n_attacks)) %>% ungroup

## add columns "x" and "y" for the x,y coordinates associated with the zones
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))

## for team 2, these need to be on the top half of the diagram
tm2 <- attack_rate$team == teams(x)[2]
attack_rate[tm2, c("x", "y")] <- dv_xy(attack_rate$start_zone, end = "upper")[tm2, ]

## plot it
dv_heatmap(attack_rate[, c("x", "y", "rate")], legend_title = "Attack rate")

## add the court diagram
dv_court(labels = teams(x))
} # }
```
