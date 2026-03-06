# Get or set the video metadata in a datavolley object

Get or set the video metadata in a datavolley object

## Usage

``` r
dv_meta_video(x)

dv_meta_video(x) <- value
```

## Arguments

- x:

  datavolley: a datavolley object as returned by
  [`dv_read()`](dv_read.md)

- value:

  string or data.frame: a string containing the path to the video file,
  or a data.frame with columns "camera" and "file"

## Value

For `dv_meta_video`, the existing video metadata. For `dv_meta_video<-`,
the video metadata value in `x` is changed

## Examples

``` r
x <- dv_read(dv_example_file())
dv_meta_video(x) ## empty dataframe
#> [1] camera file  
#> <0 rows> (or 0-length row.names)
dv_meta_video(x) <- "/path/to/my/videofile"
dv_meta_video(x)
#>   camera                  file
#> 1      0 /path/to/my/videofile
```
