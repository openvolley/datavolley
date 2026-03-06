# Fake coordinate data

Generates fake coordinate data. The DataVolley software has the
capability to accurately record court locations associated with each
action. However, not all files contain this information (it can be time
consuming to enter). This function generates fake coordinate data that
can be used for demonstration purposes.

## Usage

``` r
dv_fake_coordinates(skill, evaluation)
```

## Arguments

- skill:

  string: the skill type to generate positions for (only "serve" is
  implemented so far)

- evaluation:

  character: vector of evaluations (as returned in the `evalution`
  column of a datavolleyplays object)

## Value

data.frame of coordinates with columns "start_coordinate",
"start_coordinate_x", "start_coordinate_y", "end_coordinate",
"end_coordinate_x", "end_coordinate_y". The returned data.frame will
have as many rows as the length of the `evaluation` vector

## See also

[`dv_xy`](dv_xy.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

## read example data file
x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)

## take just the serves from the play-by-play data
xserves <- subset(plays(x), skill=="Serve")

## if the file had been scouted with coordinate included, we could plot them directly
## this file has no coordinates, so we'll fake some up for demo purposes
coords <- dv_fake_coordinates("serve", xserves$evaluation)
xserves[, c("start_coordinate", "start_coordinate_x", "start_coordinate_y",
            "end_coordinate", "end_coordinate_x", "end_coordinate_y")] <- coords

## now we can plot these
xserves$evaluation[!xserves$evaluation %in% c("Ace", "Error")] <- "Other"

ggplot(xserves, aes(start_coordinate_x, start_coordinate_y,
       xend=end_coordinate_x, yend=end_coordinate_y, colour=evaluation))+
    geom_segment() + geom_point() +
    scale_colour_manual(values=c(Ace="limegreen", Error="firebrick", Other="dodgerblue")) +
    ggcourt(labels=c("Serving team", "Receiving team"))
} # }
```
