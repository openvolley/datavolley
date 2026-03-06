# Additional validation checks on a DataVolley file

This function is automatically run as part of `dv_read` if
`extra_validation` is greater than zero. The current validation
messages/checks are:

- message "The total of the home/visiting team scores in the match
  result summary (x\$meta\$result) does not match the total number of
  points recorded for the home/visiting team in the plays data"

- message "Home/Visiting team roster is empty": the home or visiting
  team roster has not been entered

- message "Players xxx and yyy have the same player ID": player IDs
  should be unique, and so duplicated IDs will be flagged here

- message "Players xxx and yyy have the same jersey number": players on
  the same team should not have the same jersey number

- message "The listed player is not on court in this rotation": the
  player making the action is not part of the current rotation. Libero
  players are ignored for this check

- message "Back-row player made an attack from a front-row zone": an
  attack starting from zones 2-4 was made by a player in the back row of
  the current rotation

- message "Front-row player made an attack from a back-row zone (legal,
  but possibly a scouting error)": an attack starting from zones 1,5-9
  was made by a player in the front row of the current rotation

- message "Quick attack by non-middle player"

- message "Middle player made a non-quick attack"

- message "Block by a back-row player"

- message "Winning serve not coded as an ace"

- message "Non-winning serve was coded as an ace"

- message "Serving player not in position 1"

- message "Player designated as libero was recorded making a
  serve/attack/block"

- message "Attack (which was blocked) does not have number of blockers
  recorded"

- message "Attack (which was followed by a block) has 'No block'
  recorded for number of players"

- message "Repeated row with same skill and evaluation_code for the same
  player"

- message "Consecutive actions by the same player"

- message "Point awarded to incorrect team following error (or \\error\\
  evaluation incorrect)"

- message "Point awarded to incorrect team (or winning play evaluation
  incorrect)"

- message "Scores do not follow proper sequence": one or both team
  scores change by more than one point at a time

- message "Visiting/Home team rotation has changed incorrectly"

- message "Player lineup did not change after substitution: was the sub
  recorded incorrectly?"

- message "Reception type does not match serve type": the type of
  reception (e.g. "Jump-float serve reception" does not match the serve
  type (e.g. "Jump-float serve")

- message "Reception start zone does not match serve start zone"

- message "Reception end zone does not match serve end zone"

- message "Reception end sub-zone does not match serve end sub-zone"

- message "Attack type does not match set type": the type of attack
  (e.g. "Head ball attack") does not match the set type (e.g. "High ball
  set")

- message "Block type does not match attack type": the type of block
  (e.g. "Head ball block") does not match the attack type (e.g. "High
  ball attack")

- message "Dig type does not match attack type": the type of dig (e.g.
  "Head ball dig") does not match the attack type (e.g. "High ball
  attack")

- message "Multiple serves in a single rally"

- message "Multiple receptions in a single rally"

- message "Serve (that was not an error) did not have an accompanying
  reception"

- message "Rally had ball contacts but no serve"

- message "Replacement of home/visiting setter: the team is in rotation
  X but the replacement setter is not in that position"

- message "Set on perfect/good reception made by a player other than the
  designated setter (might indicate an error with the
  rotation/designated setter)"

- message "Setter call on a set made by a player other than the
  designated setter (might indicate an error with the
  rotation/designated setter)"

- "Setter call on negative reception"

- message "Set by the home/visiting team was in between a dig/reception
  and attack by the other team (was the set assigned to the correct
  team?)"

## Usage

``` r
dv_validate(x, validation_level = 2, options = list(), file_type)

validate_dv(x, validation_level = 2, options = list(), file_type)
```

## Arguments

- x:

  datavolley: datavolley object as returned by `dv_read`

- validation_level:

  numeric: how strictly to check? If 0, perform no checking; if 1, only
  identify major errors; if 2, also return any issues that are likely to
  lead to misinterpretation of data; if 3, return all issues (including
  minor issues such as those that might have resulted from selective
  post-processing of compound codes)

- options:

  list: named list of options that control optional validation
  behaviour. Valid entries are:

  - setter_tip_codes character: vector of attack codes that represent
    setter tips (or other attacks that a back-row player can validly
    make from a front-row position). If you code setter tips as attacks,
    and don't want such attacks to be flagged as an error when made by a
    back-row player in a front-row zone, enter the setter tip attack
    codes here. e.g. `options=list(setter_tip_codes=c("PP","XY"))`

- file_type:

  string: "indoor" or "beach". If not provided, will be taken from the
  `x$file_meta$file_format` entry

## Value

data.frame with columns message (the validation message),
file_line_number (the corresponding line number in the DataVolley file),
video_time, and file_line (the actual line from the DataVolley file).

## See also

[`dv_read`](dv_read.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- dv_read(dv_example_file(), insert_technical_timeouts = FALSE)
  xv <- dv_validate(x)

  ## specifying "PP" as the setter tip code
  ## front-row attacks (using this code) by a back-row player won't be flagged as errors
  xv <- dv_validate(x, options = list(setter_tip_codes = c("PP")))
} # }
```
