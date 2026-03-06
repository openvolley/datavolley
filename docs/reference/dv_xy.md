# Court zones to x, y coordinates

Generate x and y coordinates for plotting, from DataVolley numbered
zones

## Usage

``` r
dv_xy(
  zones,
  end = "lower",
  xynames = c("x", "y"),
  as_for_serve = FALSE,
  subzones
)
```

## Arguments

- zones:

  numeric: zones numbers 1-9 to convert to x and y coordinates

- end:

  string: use the "lower" or "upper" part of the figure

- xynames:

  character: names to use for the x and y columns of the returned
  data.frame

- as_for_serve:

  logical: if TRUE, treat positions as for serving. Only zones 1,5,6,7,9
  are meaningful in this case

- subzones:

  character: if supplied, coordinates will be adjusted for subzones.
  Values other than "A" to "D" will be ignored

## Value

data.frame with columns "x" and "y" (or other names if specified in
`xynames`)

## Details

For a description of the court dimensions and coordinates used for
plotting, see [`ggcourt`](ggcourt.md)

## See also

[`ggcourt`](ggcourt.md), [`dv_flip_xy`](dv_flip_xy.md),
[`dv_xy2index`](dv_index2xy.md), [`dv_index2xy`](dv_index2xy.md),
[`dv_cone2xy`](dv_cone2xy.md), [`dv_xy2zone`](dv_xy2zone.md),
[`dv_xy2subzone`](dv_xy2subzone.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)

library(ggplot2)
library(dplyr)

## Example 1: attack frequency by zone, per team

attack_rate <- plays(x) %>% dplyr::filter(skill == "Attack") %>%
  group_by(team, start_zone) %>% dplyr::summarize(n_attacks = n()) %>%
  mutate(rate = n_attacks/sum(n_attacks)) %>% ungroup

## add columns "x" and "y" for the x, y coordinates associated with the zones
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))

## for team 2, these need to be on the top half of the diagram
tm2 <- attack_rate$team == teams(x)[2]
attack_rate[tm2, c("x", "y")] <- dv_xy(attack_rate$start_zone, end = "upper")[tm2, ]

## plot this
ggplot(attack_rate, aes(x, y, fill = rate)) + geom_tile() + ggcourt(labels = teams(x)) +
  scale_fill_gradient2(name = "Attack rate")


## Example 2: map of starting and ending zones of attacks using arrows

## first tabulate attacks by starting and ending zone
attack_rate <- plays(x) %>% dplyr::filter(team == teams(x)[1] & skill == "Attack") %>%
  group_by(start_zone, end_zone) %>% tally() %>% ungroup

## convert counts to rates
attack_rate$rate <- attack_rate$n/sum(attack_rate$n)

## discard zones with zero attacks or missing location information
attack_rate <- attack_rate %>% dplyr::filter(rate>0 & !is.na(start_zone) & !is.na(end_zone))

## add starting x,y coordinates
attack_rate <- cbind(attack_rate,
     dv_xy(attack_rate$start_zone, end = "lower", xynames = c("sx","sy")))

## and ending x,y coordinates
attack_rate <- cbind(attack_rate,
     dv_xy(attack_rate$end_zone, end = "upper", xynames = c("ex","ey")))

## plot in reverse order so largest arrows are on the bottom
attack_rate <- attack_rate %>% dplyr::arrange(desc(rate))

p <- ggplot(attack_rate, aes(x, y, col = rate)) + ggcourt(labels = c(teams(x)[1],""))
for (n in 1:nrow(attack_rate))
    p <- p + geom_path(data = data.frame(x = c(attack_rate$sx[n], attack_rate$ex[n]),
                                       y = c(attack_rate$sy[n],attack_rate$ey[n]),
                                       rate = attack_rate$rate[n]),
        aes(linewidth = rate), lineend = "round", arrow = arrow(ends = "last", type = "closed"))
p + scale_fill_gradient(name = "Attack rate") + guides(linewidth = "none")
} # }
```
