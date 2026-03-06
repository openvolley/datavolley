# Plot a court heatmap, using base graphics

See `link{ggcourt}` for a `ggplot2`-based court diagram, which can be
used to plot heatmaps with e.g.
[`ggplot2::geom_tile`](https://ggplot2.tidyverse.org/reference/geom_tile.html).

## Usage

``` r
dv_heatmap(
  x,
  y,
  z,
  col,
  zlim,
  legend = TRUE,
  legend_title = NULL,
  legend_title_font = 1,
  legend_title_cex = 0.7,
  legend_cex = 0.7,
  legend_pos = c(0.8, 0.85, 0.25, 0.75),
  res,
  add = FALSE
)
```

## Arguments

- x:

  numeric, RasterLayer or data.frame: x-coordinates of the data to plot,
  or a `RasterLayer` layer or `data.frame` containing the data (x, y,
  and z together)

- y:

  numeric: y-coordinates of the data to plot

- z:

  numeric: values of the data to plot

- col:

  character: a vector of colours to use

- zlim:

  numeric: the minimum and maximum z values for which colors should be
  plotted, defaulting to the range of the finite values of z

- legend:

  logical: if `TRUE`, plot a legend

- legend_title:

  string: title for the legend

- legend_title_font:

  numeric: 1 = normal, 2 = bold, 3 = italic

- legend_title_cex:

  numeric: size scaling of legend title

- legend_cex:

  numeric: size scaling of legend text

- legend_pos:

  numeric: position of the legend (xmin, xmax, ymin, ymax) - in
  normalized units

- res:

  numeric: size of the heatmap cells. This parameter should only be
  needed in cases where the input data are sparse, when the automatic
  algorithm can't work it out. Values are given in metres, so `res` is 3
  when showing zones, or 1.5 when showing subzones

- add:

  logical: if `TRUE`, add the heatmap to an existing plot

## Details

Data can be provided either as separate `x`, `y`, and `z` objects, or as
a single `RasterLayer` or `data.frame` object. If a `data.frame`, the
first three columns are used (and assumed to be in the order `x`, `y`,
`z`).

## See also

[`dv_court`](dv_court.md), [`dv_plot_new`](dv_plot_new.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)

library(dplyr)

## Example: attack frequency by zone, per team

attack_rate <- plays(x) %>% dplyr::filter(skill == "Attack") %>%
  group_by(team, start_zone) %>%
  dplyr::summarize(n_attacks = n(), .groups = "drop_last") %>%
  mutate(rate = n_attacks / sum(n_attacks)) %>% ungroup

## add columns "x" and "y" for the x,y coordinates associated with the zones
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))

## for team 2, these need to be on the top half of the diagram
tm2 <- attack_rate$team == teams(x)[2]
attack_rate[tm2, c("x", "y")] <- dv_xy(attack_rate$start_zone, end = "upper")[tm2, ]

## plot it
dv_heatmap(attack_rate[, c("x", "y", "rate")], legend_title = "Attack rate")

## or, controlling the z-limits
dv_heatmap(attack_rate[, c("x", "y", "rate")], legend_title = "Attack rate", zlim = c(0, 1))

## add the court diagram
dv_court(labels = teams(x))

## sometimes you may need more control over the plot layout
## set up a plot with 10% bottom/top margins and 20% left/right margins
## showing the lower half of the court only
dv_plot_new(margins = c(0.05, 0.1, 0.05, 0.1), court = "lower")

## add the heatmap
dv_heatmap(attack_rate[1:6, c("x", "y", "rate")], add = TRUE)

## and the court diagram
dv_court(court = "lower")

} # }
```
