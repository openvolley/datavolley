# datavolley

An R package for reading volleyball scouting files in DataVolley format
(`*.dvw`), collected for example with
[ovscout2](https://ovscout2.openvolley.org) or the commercial
DataVolley, Click and Scout, or VolleyStation software. VolleyStation
files in `*.vsm` format are also supported.

See also:

- the [package
  vignette](https://datavolley.openvolley.org/articles/datavolley.html)
  for more examples and information
- these [code snippets](https://snippets.openvolley.org/) for volleyball
  analytics in R with datavolley and the other openvolley packages
- this [DataVolley file validator](https://apps.untan.gl/dvalidate/) and
  [suite of analytical apps](https://apps.untan.gl/), which are built on
  the datavolley package.

The [peranavolley](https://peranavolley.openvolley.org/) package
provides similar functionality for reading files scouted by the [AOC
VBStats](http://peranasports.com/software/vbstatshd/) software.

If you want to analyze your own volleyball matches, you can use the
[ovscout2](https://ovscout2.openvolley.org) application to collect the
data.

## Installation

``` r
install.packages("datavolley", repos = c("https://openvolley.r-universe.dev",
                                         "https://cloud.r-project.org"))

## or

## install.packages("remotes") ## if needed
remotes::install_github("openvolley/datavolley")
```

## Example

Read one of the example data files bundled with the package:

``` r
library(datavolley)
x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
summary(x)
#> Match summary:
#> Date: 2015-01-25
#> League: Finale mladinke
#> Teams: Braslovče (JERONČIČ ZORAN/MIHALINEC DAMIJANA)
#>        vs
#>        Nova KBM Branik (HAFNER MATJAŽ)
#> Result: 3-0 (25-16, 25-14, 25-22)
#> Duration: 67 minutes
```

## Troubleshooting

If you see unexpected behaviour, try
`dv_read(..., do_warn = TRUE, verbose = TRUE)` to obtain more diagnostic
information during the process of reading and parsing the DataVolley
file. If your player or team names are garbled (strange characters)
check the text encoding that `dv_read(..., verbose = TRUE)` tells you
that it is using - you may need to override this with the correct one
via the `encoding` parameter.
