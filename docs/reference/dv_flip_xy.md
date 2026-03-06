# Flip the x,y court coordinates

This is a convenience function that will transform coordinates from the
top half of the court to the bottom, or vice-verse.

## Usage

``` r
dv_flip_xy(x, y)

dv_flip_x(x)

dv_flip_y(y)

dv_flip_index(index)
```

## Arguments

- x:

  numeric: x-coordinate. For `dv_flip_xy` this can be a two-column
  matrix or data.frame containing x and y

- y:

  numeric: y-coordinate

- index:

  integer: grid index value

## Value

transformed coordinates or grid index

## See also

[`ggcourt`](ggcourt.md), [`dv_xy`](dv_xy.md),
[`dv_xy2index`](dv_index2xy.md), [`dv_index2xy`](dv_index2xy.md)

## Examples

``` r
if (FALSE) { # \dontrun{
 x <- dv_read(dv_example_file(), insert_technical_timeouts=FALSE)
 library(ggplot2)
 library(dplyr)

## attack rate by zone (both teams combined)
attack_rate <- plays(x) %>% dplyr::filter(skill=="Attack") %>%
   group_by(team, start_zone) %>% dplyr::summarize(n_attacks=n()) %>%
   mutate(rate=n_attacks/sum(n_attacks)) %>% ungroup

## add columns "x" and "y" for the x,y coordinates associated with the zones
attack_rate <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end="lower"))

## plot this
ggplot(attack_rate, aes(x, y, fill=rate)) + geom_tile() + ggcourt(labels=teams(x)) +
     scale_fill_gradient2(name="Attack rate")

## or, plot at the other end of the court
attack_rate <- attack_rate %>% mutate(x=dv_flip_x(x), y=dv_flip_y(y))

ggplot(attack_rate, aes(x, y, fill=rate)) + geom_tile() + ggcourt(labels=teams(x)) +
     scale_fill_gradient2(name="Attack rate")
} # }
```
