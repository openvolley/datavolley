datavolley 1.11.0
================
- the `dv_sync_video` function now does not apply offsets by default - see the function documentation for details. To restore the previous behaviour use `dv_sync_video(..., offsets = dv_sync_offsets())` but it is recommended to apply offsets in your video montage software, not during the sync process (hence the change to the default behaviour)
- small changes to the default sync timings
- fix an error in the line numbers returned by some validation checks

datavolley 1.10.0
================
- the validation step is now less strict when checking substitutions. Some files will change the players in the lineup on the line before (or in some cases, several lines before) the actual substitution code. By default these now won't be flagged, so long as everything comes out consistent before the next rally starts. Strict checking can be re-enabled via `dv_validate(..., validation_level = 3)` or `dv_read(..., extra_validation = 3)`

datavolley 1.9.0
================
- substantial refactoring of how text encoding is detected, which should be more reliable (particularly for DV4 files)
- speed improvements to file reading

datavolley 1.7.4
================
- minor revisions to the `dv_sync_video` function

datavolley 1.7.3
================
- minor bugfixes

datavolley 1.7.1
================
- the evaluation string used for a set with a "/" evaluation code has been changed from "Error (reach over net)" to just "Error" for consistency with other skills (applies to files using VolleyMetrics conventions only)

datavolley 1.7.0
================
- fixes to support files using sideout scoring

datavolley 1.6.4
================
- add experimental video sync functions

datavolley 1.6.3
================
- remove dependence on `enc` package, which has been archived from CRAN
- enforce the `start_coordinate` column in meta$attacks table to be integer
- other small improvements

datavolley 1.6.0
================
- propagate player name changes (made via `remap_player_names()`) into first and last names in metadata (thanks @awosoga)
- added `remap_player_info()` function (thanks @awosoga)
- other small bugfixes

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

