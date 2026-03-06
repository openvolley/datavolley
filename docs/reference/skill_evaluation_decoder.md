# Translate skill evaluation codes into meaningful summary phrases

If your DataVolley files use evaluation codes differently to those coded
here, you will need to supply a custom skill_evaluation_decode function
to [`dv_read`](dv_read.md)

## Usage

``` r
skill_evaluation_decoder(style = "default")
```

## Arguments

- style:

  string: currently "default" (following the standard definitions
  described in the DataVolley manual) or "volleymetrics" (per the
  conventions that VolleyMetrics use)

## Value

function. This function takes arguments skill, evaluation_code, and
show_map and returns a string giving the interpretation of that skill
evaluation code

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
sd <- skill_evaluation_decoder()
sd("S","#")
#> [1] "Ace"
sd(show_map=TRUE)
#>    skill evaluation_code                     evaluation
#> 1      S               =                          Error
#> 2      S               /            Positive, no attack
#> 3      S               - Negative, opponent free attack
#> 4      S               + Positive, opponent some attack
#> 5      S               #                            Ace
#> 6      S               !    OK, no first tempo possible
#> 7      R               =                          Error
#> 8      R               /                Poor, no attack
#> 9      R               -       Negative, limited attack
#> 10     R               +               Positive, attack
#> 11     R               #                   Perfect pass
#> 12     R               !    OK, no first tempo possible
#> 13     A               =                          Error
#> 14     A               /                        Blocked
#> 15     A               -               Poor, easily dug
#> 16     A               !           Blocked for reattack
#> 17     A               +          Positive, good attack
#> 18     A               #                 Winning attack
#> 19     B               =                          Error
#> 20     B               /                       Invasion
#> 21     B               -     Poor, opposition to replay
#> 22     B               +          Positive, block touch
#> 23     B               #                  Winning block
#> 24     B               !     Poor, opposition to replay
#> 25     D               =                          Error
#> 26     D               /    Ball directly back over net
#> 27     D               -  No structured attack possible
#> 28     D               #                    Perfect dig
#> 29     D               +                       Good dig
#> 30     D               !    OK, no first tempo possible
#> 31     E               =                          Error
#> 32     E               -                           Poor
#> 33     E               /                           Poor
#> 34     E               +                       Positive
#> 35     E               #                        Perfect
#> 36     E               !                             OK
#> 37     F               =                          Error
#> 38     F               /                           Poor
#> 39     F               !    OK, no first tempo possible
#> 40     F               -     OK, only high set possible
#> 41     F               +                           Good
#> 42     F               #                        Perfect
```
