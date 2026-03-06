# Summarize the video sync times in a dvw file

This function will generate a summary of various video time differences
in a dvw file. Apply this to a file that you have synchronized to video,
and the results can be used to tweak the behaviour of
[`dv_sync_video`](dv_sync_video.md).

## Usage

``` r
dv_sync_summary(x)
```

## Arguments

- x:

  datavolley: a single datavolley object as returned by
  [`dv_read`](dv_read.md), or the `plays` component of one

## Value

A data.frame with columns `type`, `N`, `mean`, `most_common`, `min`,
`max`

## See also

[`dv_sync_video`](dv_sync_video.md)

## Examples

``` r
x <- dv_read(dv_example_file(3))
dv_sync_summary(x)
#> # A tibble: 14 × 6
#>    type                                          N  mean most_common   min   max
#>    <chr>                                     <int> <dbl>       <dbl> <dbl> <dbl>
#>  1 Serve to reception time (Jump serve)         27 2.85            3     1     6
#>  2 Serve to reception time (Jump-float serv…    48 2.02            1     0     4
#>  3 Serve to reception time (Other serve typ…    22 1.09            0    -1     3
#>  4 Reception to set time                        72 2.83            3     0    11
#>  5 Set to attack time (High ball set)           54 0.889           0    -1     3
#>  6 Set to attack time (Other set types)         57 1.04            1    -1     3
#>  7 Set to attack time (Quick ball set)          16 0.812           2    -5     3
#>  8 Attack to block time                         40 1.8             3     0     7
#>  9 Attack to dig time (no block touch)          45 1.78            1    -1     6
#> 10 Attack to dig time (with block touch)         3 3               6     0     6
#> 11 Dig to set time                              39 0.513           1    -2     2
#> 12 Attack to counter-attack time                48 3.69            3     0     6
#> 13 Overpass to next touch time                  13 4.15            5     0     8
#> 14 Last touch to end of rally                  107 0               0     0     0
```
