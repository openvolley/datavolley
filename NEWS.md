datavolley 0.13.0
=================

- adjustments to `team_touch_id` and `phase` to account for files in which only attacks have been scouted
- `point_phase` (this is the "Point/Side out" column in the DataVolley codes list window) and `attack_phase` ("Attack after reception/dig") columns added to the play-by-play data
- `dv_write` function added
- additional fixes following the changes to text encoding handling in v0.12.0

datavolley 0.12.0
=================

- substantial changes to the way that text encoding is handled, including guessing the correct encoding with `read_dv(..., text_encoding = "guess")`. Text encoding should work better now, particular on Windows
- added a `date_format` parameter to `read_dv`, so that the user can specify the preferred date format to use when dates are ambiguous

