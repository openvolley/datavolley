# Generate a short, human-readable text summary of one or more actions

Generate a short, human-readable text summary of one or more actions

## Usage

``` r
dv_action2text(x, verbosity = 1)
```

## Arguments

- x:

  data.frame or tibble: one or more rows from a datavolleyplays object
  as returned by [`dv_read`](dv_read.md)

- verbosity:

  integer: 1 = least verbose, 2 = more verbose. Currently ignored

## Value

character vector

## Examples

``` r
x <- dv_read(dv_example_file())
dv_action2text(plays(x)[27:30, ])
#> [1] "Float serve by ANJA HRIBERNIK 97 (Negative, opponent free attack)"
#> [2] "Reception by KARMINA SUŠNIK (Perfect pass)"                       
#> [3] "Set by KAJA KEGLEVIČ (Perfect)"                                   
#> [4] "Head ball attack by ANITA SOBOČAN (X5 - Error)"                   
```
