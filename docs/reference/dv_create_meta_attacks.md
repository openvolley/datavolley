# Create a meta attack data.frame from the plays object if it is missing

If your DataVolley file does not have a meta attack dataframe (for
example, if you are using Click&Scout), this function will create one
from the information in the plays object.

## Usage

``` r
dv_create_meta_attacks(plays)
```

## Arguments

- plays:

  data.frame: the plays component of a datavolley object, as returned by
  [`dv_read`](dv_read.md)

## Value

A data.frame of attacks.
