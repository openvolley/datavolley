datavolley 1.5.0
================
- support for .vsm files

datavolley 1.3.5
================
Minor updates:
- better detection of Cyrillic text encoding

datavolley 1.3.3
================
Minor updates:
- initial setter assignments (at the start of each set) are no longer marked as substitutions
- better handling of corrupted position coordinates in dvw files

datavolley 1.3.0
================
- the raster package has been shifted from Imports to Suggests, meaning that it won't be installed by default. This only affects plotting of raster images (heatmaps) in base graphics. If you don't have raster installed, it will prompt

datavolley 1.2.9
=================
- convert colour columns to hex colour strings (previously integer-encoded)
- minor speed improvements when reading
- meta$winning_symbols is now a data frame

datavolley 1.2.3
=================
- utilities to make files scouted by ClickNScout more useful
- optional xlim, ylim to ggcourt,
- add dv_find_to_flip_coordinates function

datavolley 1.2.0
=================
- bug fix: actions scouted with an unknown player (denoted in the dvw file with player number "$$") were previously being incorrectly parsed

datavolley 1.0.0
=================
- cope with missing file metadata entries
- improvements for handling text encoding

datavolley 1.0.0
=================
- fix dv_xy2zone to make serve zones equal widths
- add line_width to ggcourt

datavolley 0.16.0
=================
- add dv_read_sq function
- improvements to match date parsing
- add columns for scores at start of point (home_score_start_of_point, visiting_score_start_of_point)
- trim whitespace from team and player names and IDs

datavolley 0.15.0
=================
- minor bugfixes for rotation check, validation of peranavolley files
- 'beach' and 'sand' ggcourt options
- fix set number and sets_won in partially-scouted files

datavolley 0.14.0
=================
- improvements to detecting text encoding
- minor bugfixes

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

