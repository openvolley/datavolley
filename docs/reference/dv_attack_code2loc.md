# Nominal starting coordinate for standard attack codes

Nominal starting coordinate for standard attack codes

## Usage

``` r
dv_attack_code2loc(code)
```

## Arguments

- code:

  character: vector of attack codes ("X5", "VP", etc)

## Value

A vector of numeric coordinates

## Examples

``` r
dv_attack_code2loc(code = c("X5", "X7", "PP"))
#>   X5   X7   PP 
#> 4912 4932 4964 
```
