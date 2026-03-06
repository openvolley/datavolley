# Read a team roster (\*.sq) file

Read a team roster (\*.sq) file

## Usage

``` r
dv_read_sq(
  filename,
  do_transliterate = FALSE,
  encoding = "guess",
  date_format = "guess",
  surname_case = "asis",
  verbose = FALSE
)
```

## Arguments

- filename:

  string: file name to read

- do_transliterate:

  logical: should we transliterate all text to ASCII?

- encoding:

  character: text encoding to use. Text is converted from this encoding
  to UTF-8. A vector of multiple encodings can be provided, and this
  function will attempt to choose the best. If encoding is "guess", the
  encoding will be guessed

- date_format:

  string: the expected date format (used for dates of birth). One of
  "ymd", "mdy", "dmy", or "guess". If `date_format` is something other
  than "guess", that date format will be preferred where dates are
  ambiguous

- surname_case:

  string or function: should we change the case of player surnames? If
  `surname_case` is a string, valid values are "upper","lower","title",
  or "asis"; otherwise `surname_case` may be a function that will be
  applied to the player surname strings

- verbose:

  logical: if `TRUE`, show progress

## Value

A list with two components: "team" and "players", both of which are data
frames

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read_sq("/path/to/my/roster_file")
} # }
```
