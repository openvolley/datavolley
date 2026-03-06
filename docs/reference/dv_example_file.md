# Example DataVolley files provided as part of the datavolley package

Example DataVolley files provided as part of the datavolley package

## Usage

``` r
dv_example_file(choice = 1)
```

## Arguments

- choice:

  numeric: which data file to return?

  - 1 - the January 2015 Slovenian junior women's final between
    Braslovče and Nova KBM Branik (obtained from
    <http://www.odbojka.si/>

  - 2 - the December 2012 men's Slovenian national championship
    semifinal between ACH Volley and Maribor (obtained from
    <http://www.odbojka.si/>

  - 3 - Nicaragua vs Cuba women from the Pan American Cup, August 2022
    (vsm format, courtesy Christophe Elek)

## Value

path to the file

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
myfile <- dv_example_file()
x <- dv_read(myfile, insert_technical_timeouts = FALSE)
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
