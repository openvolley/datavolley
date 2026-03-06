# ggplot volleyball court

Volleyball court schematic suitable for adding to a ggplot

## Usage

``` r
ggcourt(
  court = "full",
  show_zones = TRUE,
  labels = c("Serving team", "Receiving team"),
  as_for_serve = FALSE,
  show_zone_lines = TRUE,
  show_minor_zones = FALSE,
  show_3m_line = TRUE,
  grid_colour = "black",
  zone_colour = "grey70",
  minor_zone_colour = "grey80",
  fixed_aspect_ratio = TRUE,
  zone_font_size = 10,
  label_font_size = 12,
  label_colour = "black",
  court_colour = NULL,
  figure_colour = NULL,
  background_only = FALSE,
  foreground_only = FALSE,
  line_width = 0.5,
  xlim,
  ylim,
  ...
)
```

## Arguments

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

- show_3m_line:

  logical: if TRUE, show the 3m (10ft) line

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

- label_font_size:

  numeric: the font size of the labels

- label_colour:

  string: colour to use for labels

- court_colour:

  string: colour to use for the court. If `NULL`, the court is only
  plotted with lines (no colour fill) and so the `figure_colour` will
  show through. Several special values are also supported here:

  - `court_colour = "indoor"` can be used as a shortcut to set the court
    colour to orange, figure colour to blue, and lines and labels to
    white (similar to the typical indoor court colour scheme)

  - `court_colour = "beach"` can be used as a shortcut to set the court
    and figure colour to a sandy-coloured yellow, lines and labels to
    black, and with the 3m line not shown by default

  - `court_colour = "sand"` as for "beach" but with a sand texture image
    used as the court background

- figure_colour:

  string: colour to set the figure background to. If `NULL`, the
  background colour of the theme will be used (white, by default)

- background_only:

  logical: if `TRUE`, only plot the background elements (including
  general plot attributes such as the theme)

- foreground_only:

  logical: if `TRUE`, only plot the foreground elements (grid lines,
  labels, etc)

- line_width:

  numeric: line width (passed as the size parameter to e.g.
  [`ggplot2::geom_path`](https://ggplot2.tidyverse.org/reference/geom_path.html))

- xlim:

  numeric: (optional) limits for the x-axis

- ylim:

  numeric: (optional) limits for the y-axis

- ...:

  : additional parameters passed to
  [`ggplot2::theme_classic`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

## Value

ggplot layer

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

[`dv_xy`](dv_xy.md), [`dv_xy2index`](dv_index2xy.md),
[`dv_index2xy`](dv_index2xy.md), [`dv_flip_xy`](dv_flip_xy.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)

library(ggplot2)
library(dplyr)

## Example 1: attack frequency by zone, per team

attack_rate <- plays(x) %>% dplyr::filter(skill == "Attack") %>%
  group_by(team, start_zone) %>% dplyr::summarize(n_attacks=n()) %>%
  mutate(rate=n_attacks/sum(n_attacks)) %>% ungroup

## add columns "x" and "y" for the x,y coordinates associated with the zones
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))

## for team 2, these need to be on the top half of the diagram
tm2 <- attack_rate$team == teams(x)[2]
attack_rate[tm2, c("x", "y")] <- dv_xy(attack_rate$start_zone, end = "upper")[tm2, ]

## plot this
ggplot(attack_rate, aes(x, y, fill = rate)) + geom_tile() + ggcourt(labels = teams(x)) +
  scale_fill_gradient2(name = "Attack rate")


## Example 2: controlling layering
## use the background_only and foreground_only parameters to control the
##   order of layers in a plot

ggplot(attack_rate, aes(x, y, fill = rate)) +
  ## add the background court colours
  ggcourt(court_colour = "indoor", background_only = TRUE) +
  ## now the heatmap
  geom_tile() +
  ## and finally the grid lines and labels
  ggcourt(labels = teams(x), foreground_only = TRUE, court_colour = "indoor")


## Example 3: map of starting and ending zones of attacks using arrows

## first tabulate attacks by starting and ending zone
attack_rate <- plays(x) %>% dplyr::filter(team == teams(x)[1] & skill == "Attack") %>%
  group_by(start_zone, end_zone) %>% tally() %>% ungroup

## convert counts to rates
attack_rate$rate <- attack_rate$n/sum(attack_rate$n)

## discard zones with zero attacks or missing location information
attack_rate <- attack_rate %>% dplyr::filter(rate>0 & !is.na(start_zone) & !is.na(end_zone))

## add starting x,y coordinates
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower",
                                        xynames = c("sx","sy")))

## and ending x,y coordinates
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$end_zone, end = "upper",
                                        xynames = c("ex","ey")))

## plot in reverse order so largest arrows are on the bottom
attack_rate <- attack_rate %>% dplyr::arrange(desc(rate))

p <- ggplot(attack_rate, aes(x, y, col = rate)) + ggcourt(labels = c(teams(x)[1], ""))
for (n in 1:nrow(attack_rate))
    p <- p + geom_path(data = data.frame(x = c(attack_rate$sx[n], attack_rate$ex[n]),
                                         y = c(attack_rate$sy[n], attack_rate$ey[n]),
                                         rate = attack_rate$rate[n]),
                       aes(linewidth = rate), lineend = "round",
                       arrow = arrow(length = unit(2, "mm"), type = "closed",
                                     angle = 20, ends = "last"))
p + scale_colour_gradient(name = "Attack rate") + guides(linewidth = "none")
} # }
```
