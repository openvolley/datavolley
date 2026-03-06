# Translate attack type and starting zone into an attack code.

If your DataVolley files does not have attack codes ready, (for example,
if you are using Click&Scout), this function will take the starting zone
and tempo of the attack to map it to an attack code.

## Usage

``` r
dv_attack_code_map(type, start_zone)
```

## Arguments

- type:

  character: vector of attack tempos ("H", "T", "Q", etc). A `type`
  vector of length 1 will be expanded to the length of the `start_zone`
  vector, if needed

- start_zone:

  integer: vector of start zones

## Value

A vector of attack codes, set_types, etc.

## Examples

``` r
dv_attack_code_map(type = c("H", "Q", "T"), start_zone = c("8", "3", "4"))
#> [1] "VP" "X1" "X5"
```
