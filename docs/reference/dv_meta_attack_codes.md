# Extract the attack codes table from a datavolley object

Extract the attack codes table from a datavolley object

## Usage

``` r
dv_meta_attack_codes(x)
```

## Arguments

- x:

  datavolley: as returned by [`dv_read()`](dv_read.md)

## Value

The attack codes table from the metadata section of `x`

## Details

Note that some columns are placeholders (named e.g. `X6`, `X10` and
generally filled with NAs). These are kept for compatibility reasons.

## Examples

``` r
x <- dv_read(dv_example_file())

## the attack codes in this file
dv_meta_attack_codes(x)
#> # A tibble: 46 × 11
#>    code  attacker_position side  type  description X6    colour start_coordinate
#>    <chr>             <dbl> <chr> <chr> <chr>       <lgl> <chr>             <int>
#>  1 XF                    2 L     Q     Quick lowe… NA    #FF00…             4976
#>  2 X2                    2 L     Q     Quick set … NA    #FF00…             4868
#>  3 X1                    3 R     Q     Quick       NA    #FF00…             4956
#>  4 XM                    3 C     Q     Quick in 3  NA    #FF00…             4949
#>  5 XG                    3 R     Q     7-1 Gun     NA    #FF00…             4946
#>  6 XC                    3 R     Q     Quick far … NA    #FF00…             4947
#>  7 XD                    3 R     Q     DubleC      NA    #FF00…             4941
#>  8 X7                    4 R     Q     Quick lowe… NA    #FF00…             4932
#>  9 PP                    3 L     O     Setter tip  NA    #FF00…             4964
#> 10 X9                    4 R     M     Mezza dava… NA    #FF00…             4924
#> # ℹ 36 more rows
#> # ℹ 3 more variables: set_type <chr>, X10 <dbl>, X11 <lgl>
```
