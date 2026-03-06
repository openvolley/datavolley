# Read a DataVolley or VolleyStation file

The `do_transliterate` option may be helpful when trying to work with
multiple files from the same competition, since different text encodings
may be used on different files. This can lead to e.g. multiple versions
of the same team name. Transliterating can help avoid this, at the cost
of losing e.g. diacriticals. Transliteration is applied after converting
from the specified text encoding to UTF-8. Common encodings used with
DataVolley files include "windows-1252" (western Europe), "windows-1250"
(central Europe), "iso-8859-1" (western Europe and Americas),
"iso-8859-2" (central/eastern Europe), "iso-8859-13" (Baltic languages)

## Usage

``` r
dv_read(
  filename,
  insert_technical_timeouts = TRUE,
  do_warn = FALSE,
  do_transliterate = FALSE,
  encoding = "guess",
  date_format = "guess",
  extra_validation = 2,
  validation_options = list(),
  surname_case = "asis",
  skill_evaluation_decode = "default",
  custom_code_parser,
  metadata_only = FALSE,
  verbose = FALSE,
  edited_meta
)

read_dv(
  filename,
  insert_technical_timeouts = TRUE,
  do_warn = FALSE,
  do_transliterate = FALSE,
  encoding = "guess",
  date_format = "guess",
  extra_validation = 2,
  validation_options = list(),
  surname_case = "asis",
  skill_evaluation_decode = "default",
  custom_code_parser,
  metadata_only = FALSE,
  verbose = FALSE,
  edited_meta
)
```

## Arguments

- filename:

  string: file name to read

- insert_technical_timeouts:

  logical or list: should we insert technical timeouts? If TRUE,
  technical timeouts are inserted at points 8 and 16 of sets 1–4 (for
  indoor files) or when the team scores sum to 21 in sets 1–2 (beach).
  Otherwise a two-element list can be supplied, giving the scores at
  which technical timeouts will be inserted for sets 1–4, and set 5.

- do_warn:

  logical: should we issue warnings about the contents of the file as we
  read it?

- do_transliterate:

  logical: should we transliterate all text to ASCII? See details

- encoding:

  character: text encoding to use. Text is converted from this encoding
  to UTF-8. A vector of multiple encodings can be provided, and this
  function will attempt to choose the best. If encoding is "guess", the
  encoding will be guessed

- date_format:

  string: the expected date format (one of "ymd", "mdy", or "dmy") or
  "guess". If `date_format` is something other than "guess", that date
  format will be preferred where dates are ambiguous

- extra_validation:

  numeric: should we run some extra validation checks on the file? 0=no
  extra validation, 1=check only for major errors, 2=somewhat more
  extensive, 3=the most extra checking

- validation_options:

  list: additional options to pass to the validation step. See
  [`dv_validate()`](dv_validate.md) for details

- surname_case:

  string or function: should we change the case of player surnames? If
  `surname_case` is a string, valid values are "upper","lower","title",
  or "asis"; otherwise `surname_case` may be a function that will be
  applied to the player surname strings

- skill_evaluation_decode:

  function or string: if `skill_evaluation_decode` is a string, it can
  be either "default" (use the default DataVolley conventions for dvw or
  vsm files), "volleymetrics" (to follow the scouting conventions used
  by VolleyMetrics), "german" (same as "default" but with B/ and B=
  swapped), or "guess" (use volleymetrics if it looks like a
  VolleyMetrics file, otherwise default). If `skill_evaluation_decode`
  is a function, it should convert skill evaluation codes into
  meaningful phrases. See
  [`skill_evaluation_decoder()`](skill_evaluation_decoder.md)

- custom_code_parser:

  function: function to process any custom codes that might be present
  in the datavolley file. This function takes one input (the
  `datavolley` object) and should return a list with two named
  components: `plays` and `messages`

- metadata_only:

  logical: don't process the plays component of the file, just the match
  and player metadata

- verbose:

  logical: if TRUE, show progress

- edited_meta:

  list: if supplied, will be used in place of the metadata present in
  the file itself. This makes it possible to, for example, read a file,
  edit the metadata, and re-parse the file but using the modified
  metadata

## Value

A named list with several elements. `meta` provides match metadata,
`plays` is the main play-by-play data in the form of a data.frame. `raw`
is the line-by-line content of the datavolley file. `messages` is a
data.frame describing any inconsistencies found in the file.

## References

<http://www.dataproject.com/IT/en/Volleyball>

## See also

[`skill_evaluation_decoder()`](skill_evaluation_decoder.md),
[`dv_validate()`](dv_validate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  ## to read the example file bundled with the package
  myfile <- dv_example_file()
  x <- dv_read(myfile, insert_technical_timeouts=FALSE)
  summary(x)

  ## or to read your own file:
  x <- dv_read("c:/some/path/myfile.dvw", insert_technical_timeouts=FALSE)

  ## Insert a technical timeout at point 12 in sets 1 to 4:
  x <- dv_read(myfile, insert_technical_timeouts=list(c(12),NULL))

  ## to read a VolleyMetrics file
  x <- dv_read(myfile, skill_evaluation_decode = "volleymetrics")
} # }
```
