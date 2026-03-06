# Set type for standard attack codes

Set type for standard attack codes

## Usage

``` r
dv_attack_code2set_type(code)
```

## Arguments

- code:

  character: vector of attack codes ("X5", "VP", etc)

## Value

A named vector of sides ("F", "B", "C", "P", "S", "-")

## Examples

``` r
dv_attack_code2set_type(code = c("X5", "X7", "PP"))
#>  X5  X7  PP 
#> "F" "C" "S" 
```
