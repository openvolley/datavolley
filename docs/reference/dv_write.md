# Write a datavolley object to dvw file

Note that this is really rather experimental, and you probably shouldn't
use it yet. Once complete, this function will allow a datavolley file to
be read in via [`dv_read`](dv_read.md), modified by the user, and then
rewritten back to a datavolley file. At this stage, most modifications
to the datavolley object should make it back into the rewritten file.
However, the scouted code (in the `code` column) is NOT yet updated to
reflect changes that might have been made to other columns in the
datavolley object.

## Usage

``` r
dv_write(x, file, text_encoding = "UTF-8")

write_dv(x, file, text_encoding = "UTF-8")
```

## Arguments

- x:

  datavolley: a datavolley object as returned by [`dv_read`](dv_read.md)

- file:

  string: the filename to write to. If not supplied, no file will be
  written but the dvw content will be returned

- text_encoding:

  string: the text encoding to use

## Value

The dvw file contents as a character vector (invisibly)

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file())
  outfile <- tempfile()
  dv_write(x, outfile)
} # }
```
