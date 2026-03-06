# Remap attack codes to new values

Remap attack codes to new values

## Usage

``` r
dv_remap_attack_codes(x, from_codes, to_codes, allow_remove = FALSE)
```

## Arguments

- x:

  datavolley: as returned by [`dv_read()`](dv_read.md)

- from_codes:

  character: one or more attack codes to change

- to_codes:

  character: one or more replacement attack codes. `to_codes` must
  either be the same length as `from_codes` (in which case each element
  of `from_codes` will be changed to its corresponding element in
  `to_codes`), or `to_codes` must be a single string (in which case each
  element of `from_codes` will be changed to this code)

- allow_remove:

  logical: if a given value of `to_code` is NA or an empty string (""),
  the corresponding `from_code` will be removed, so long as
  `allow_remove` is TRUE. It is not entirely clear how useful this
  removal functionality is, so the extra `allow_remove` parameter must
  be set in order to avoid inadvertant removals

## Value

A modified copy of `x`

## Details

If a given value of `to_code` does not exist in the attacks metadata
table in `x`, we will just rename `from_code` to `to_code` without
changing any other details. If `to_code` does exist in the attacks
metadata table, we will rename the code and also change the attack
description, attack and set tempo (`skill_type` column) and set target
(`set_type`), if those details are different for the `to_code`. The
`start_zone` is not updated in either case.

If a given value of `to_code` is NA or an empty string (""), the
corresponding `from_code` will be removed, so long as `allow_remove` is
TRUE. It is not entirely clear how useful this removal functionality is,
so the extra `allow_remove` parameter must be set in order to avoid
inadvertant removals.

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

## and their usage in the play-by-play data
table(plays(x)$attack_code)
#> 
#> CB CD CF II JJ PP PR V0 V4 V5 V6 V8 V9 VI VJ VO VP VV X1 X3 X4 X5 X6 X8 X9 XC 
#>  1  3  2  2  5  8 12  2  2 31 17  8  2  6 12  2  7  8  9  2  3 41  9  4  2  2 
#> XO XP 
#>  1  7 

## Example 1
## relabel V5 and V6 as Z5 and Z6
## (leaving their other details unchanged, because Z5 and Z6
##  do not exist in the attack codes table)
x2 <- dv_remap_attack_codes(x, from_codes = c("V5", "V6"), to_codes = c("Z5", "Z6"))

## check that the table has changed
dv_meta_attack_codes(x2)
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
## and their usage in the play-by-play data
table(plays(x2)$attack_code)
#> 
#> CB CD CF II JJ PP PR V0 V4 V8 V9 VI VJ VO VP VV X1 X3 X4 X5 X6 X8 X9 XC XO XP 
#>  1  3  2  2  5  8 12  2  2  8  2  6 12  2  7  8  9  2  3 41  9  4  2  2  1  7 
#> Z5 Z6 
#> 31 17 

## Example 2
## relabel "C5" and "JJ" to "X5" (they are all fast sets to position 4)
ac <- dv_meta_attack_codes(x)
ac[ac$code %in% c("X5", "C5", "JJ"), ]
#> # A tibble: 3 × 11
#>   code  attacker_position side  type  description  X6    colour start_coordinate
#>   <chr>             <dbl> <chr> <chr> <chr>        <lgl> <chr>             <int>
#> 1 X5                    4 R     T     Shoot in 4   NA    #FF00…             4912
#> 2 C5                    4 R     U     Shoot set i… NA    #0000…             4912
#> 3 JJ                    4 R     M     Attak 4 aw   NA    #0000…             4619
#> # ℹ 3 more variables: set_type <chr>, X10 <dbl>, X11 <lgl>

x2 <- dv_remap_attack_codes(x, from_codes = c("C5", "JJ"), to_codes = "X5")
ac2 <- dv_meta_attack_codes(x2)
ac2[ac2$code %in% c("X5", "C5", "JJ"), ]
#> # A tibble: 1 × 11
#>   code  attacker_position side  type  description X6    colour  start_coordinate
#>   <chr>             <dbl> <chr> <chr> <chr>       <lgl> <chr>              <int>
#> 1 X5                    4 R     T     Shoot in 4  NA    #FF0000             4912
#> # ℹ 3 more variables: set_type <chr>, X10 <dbl>, X11 <lgl>
table(plays(x2)$attack_code)
#> 
#> CB CD CF II PP PR V0 V4 V5 V6 V8 V9 VI VJ VO VP VV X1 X3 X4 X5 X6 X8 X9 XC XO 
#>  1  3  2  2  8 12  2  2 31 17  8  2  6 12  2  7  8  9  2  3 46  9  4  2  2  1 
#> XP 
#>  7 
```
