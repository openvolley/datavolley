# Attack side for standard attack codes

Attack side for standard attack codes

## Usage

``` r
dv_attack_code2side(code)
```

## Arguments

- code:

  character: vector of attack codes ("X5", "VP", etc)

## Value

A named vector of sides ("L", "R", "C")

## Examples

``` r
dv_attack_code2side(code = c("X5", "X7", "PP"))
#>  X5  X7  PP 
#> "R" "R" "L" 
```
