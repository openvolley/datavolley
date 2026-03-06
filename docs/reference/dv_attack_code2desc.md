# Nominal descriptions for standard attack codes

Nominal descriptions for standard attack codes

## Usage

``` r
dv_attack_code2desc(code)
```

## Arguments

- code:

  character: vector of attack codes ("X5", "VP", etc)

## Value

A named character vector of descriptions. Unrecognized attack codes will
have `NA` description.

## Examples

``` r
dv_attack_code2desc(c("X5", "X7", "PP", "blah"))
#>             X5             X7             PP           blah 
#>   "Shoot in 4" "Quick - push"   "Setter tip"             NA 
```
