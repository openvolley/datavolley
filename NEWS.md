datavolley 0.12.0
=================

- substantial changes to the way that text encoding is handled, including guessing the correct encoding with `read_dv(..., text_encoding = "guess")`. Text encoding should work better now, particular on Windows
- added a `date_format` parameter to `read_dv`, so that the user can specify the preferred date format to use when dates are ambiguous

