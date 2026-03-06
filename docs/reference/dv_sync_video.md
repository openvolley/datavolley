# Synchronize video times

This function uses the time of each serve and some rules to align the
other contacts in a rally with their (approximately correct) times in
the corresponding match video. Warning: experimental!

## Usage

``` r
dv_sync_video(
  x,
  first_serve_contact,
  freeball_dig_time_offset = NA,
  contact_times = dv_sync_contact_times(),
  offsets = NULL,
  times_from,
  enforce_order = TRUE
)

dv_sync_contact_times(...)

dv_sync_offsets(...)
```

## Arguments

- x:

  datavolley: a single datavolley object as returned by
  [`dv_read`](dv_read.md)

- first_serve_contact:

  numeric or string: the video time of the first serve contact. This can
  be a numeric value giving the time in seconds from the start of the
  video, or a string of the form "MM:SS" (minutes and seconds) or
  "HH:MM:SS" (hours, minutes and seconds)

- freeball_dig_time_offset:

  numeric: if non-NA, the clock times of freeball digs will be used
  directly in the synchronization process. Freeball digs will be aligned
  using their clock times relative to the first serve contact clock
  time, with this `freeball_dig_time_offset` value (in seconds) added.
  So if when scouting live you typically enter freeball digs one second
  after they happen, use `freeball_dig_time_offset = -1`. If
  `freeball_dig_time_offset` is NA, which is the default, the clock
  times of freeball digs will not be used in the synchronization process

- contact_times:

  list: a set of parameters that control the synchronization process.
  See Details, below

- offsets:

  list: a list set of offsets to be added to each contact time in the
  second step of the synchronization process. See Details, below. If
  `offsets` is NULL or an empty list, no offsets are applied

- times_from:

  string: either "clock" or "video": take the serve times (and freeball
  dig times, if `freeball_dig_time_offset` is non-NA) from clock or
  video times. By default, clock times are used unless they are all
  missing

- enforce_order:

  logical: the estimated contact times will always be time-ordered (the
  contact time of a given touch cannot be prior to the contact time of a
  preceding touch). But the offsets can be different for different
  skills, leading to final video times that are not time ordered. These
  will be fixed if `enforce_order` is TRUE

- ...:

  : name-value pairs of elements to override the defaults in
  `dv_sync_contact_times` and `dv_sync_offsets`

## Value

A copy of `x` with modified `video_time` values in its `plays` component

## Details

When a match is scouted live, the clock time of each serve will usually
be correct because the scout can enter the serve code at the actual time
of serve. But the remainder of the touches in the rally might not be at
their correct times if the scout can't keep up with the live action.
This function makes some assumptions about typical contact-to-contact
times to better synchronize the scouted contacts with the corresponding
match video.

The clock time of each serve will be used as the reference time for each
rally (unless the user specifies `times_from = "video"`). If clock times
are not present in the file, the video time of each serve will be used
instead. If those are also missing, the function will fail.

Freeball digs can optionally be treated in the same way as serves, with
their scouted times used directly in the synchronization process.
Obviously this only makes sense if the scout has actually been
consistent in their timing when entering freeball digs, but assuming
that is the case then setting the `freeball_dig_time_offset` to a non-NA
value will improve the synchronization of rallies with freeballs. These
rallies otherwise tend to synchronize poorly, because the play is messy
and less predictable compared to in-system rallies.

Note that synchronization from clock times relies on the serve clock
times in the file being consistent, and so it will only work if the
match has been scouted in a single sitting (either live, or from video
playback but without pausing/rewinding/fast-forwarding the video). If
your clock times are not consistent but the video time of each serve is
correct, then you can use the video time of each serve as the reference
time instead.

The synchronization is a two-step process. In the first step, the video
time of each scouted contact is estimated (i.e. the actual time that the
player made contact with the ball). In the second step, skill-specific
offsets are added to those contact times. This is important if your
video montage software does not apply its own offsets, because you will
normally want a video clip to start some seconds before the actual
contact of interest. However, most video montage software - including
the openvolley ovva package and Shiny app - provide their own offsets
and so by default no offsets are applied here (this is a change: prior
to June 2025 offsets were applied here by default).

The `contact_times` object contains a set of times (in seconds), which
you can adjust to suit your scouting style and level of play. If you
have an already-synchronized dvw file, the
[`dv_sync_summary`](dv_sync_summary.md) function can provide some
guidance as to what these values should be. The `contact_times` object
contains the following entries:

- SQ - time between the scouted serve time and actual serve contact for
  jump serves

- SM - time between the scouted serve time and actual serve contact for
  jump-float serves

- SO - time between the scouted serve time and actual serve contact for
  all other serves

- SQ_R, SM_R, SO_R - the time between serve contact and reception
  contact for jump, jump-float, and other serves

- R_E - the time between reception contact and set contact

- EQ_A - the time between set contact and attack contact for quick sets

- EH_A - the time between set contact and attack contact for high sets

- EO_A - the time between set contact and attack contact for all other
  sets

- A_B - the time between attack contact and block contact

- A_D - the time between attack contact and dig contact (no intervening
  block touch)

- A_B_D - the time between attack contact and dig contact (with block
  touch)

- D_E - the time between dig contact and set contact

- RDov - the time between reception or dig overpass contact and the next
  touch by the opposition

- END - the time between the last contact and end-of-rally marker

The `offsets` object defines the offset (in seconds) to be added to each
contact time in the second pass of the synchronization process. It
contains the entries "S" (serve), "R" (reception), "E" (set), "A"
(attack), "D", (dig), "B" (block), and "F" (freeball). The `offsets`
object can be constructed using the `dv_sync_offsets` function.

Note that the entries in `contact_times` and `offsets` can be fractions.
The actual video time entries in the returned file are required to be
integers and so the final values will be rounded, but using fractional
values (particularly for the `contact_times` entries) can give better
accuracy in the intermediate calculations.

## See also

[`dv_sync_summary`](dv_sync_summary.md)

## Examples

``` r
x <- dv_read(dv_example_file())
## first serve contact was at 54s in the video
x <- dv_sync_video(x, first_serve_contact = 54)

## with a custom configuration
my_contact_times <- dv_sync_contact_times(SQ = 2) ## override default entries as necessary
## first serve contact was at 3:35 in the video
x <- dv_sync_video(x, first_serve_contact = "3:35", contact_times = my_contact_times)
```
