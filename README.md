# datavolley
An R package for reading DataVolley scouting files.

Beware, this is in an early stage of development and may do strange things. In particular, character encoding may not be well handled. Functionality is liable to change without warning.

## Installation

```R
library(devtools)
install_github("raymondben/datavolley")
```

## Example

Read the example data file bundled with the package:
```R
x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"),insert_technical_timeouts=FALSE)
summary(x)
```

    Match summary:
    Date: 2015-01-25
    League: Finale mladinke
    Teams: Braslovče (JERONČIČ ZORAN/MIHALINEC DAMIJANA)
           vs
           Nova KBM Branik (HAFNER MATJAŽ/)
    Result: 3-0 (25-16, 25-14, 25-22)
    Duration: 67 minutes


Number of serves by team:

```R
serve_idx <- find_serves(plays(x))
table(plays(x)$team[serve_idx])
```

      Braslovče Nova KBM Branik 
             74              54 


Distribution of serve run lengths:

```R
serve_run_info <- find_runs(plays(x)[serve_idx,])
table(unique(serve_run_info[,c("run_id","run_length")])$run_length)
```

     1  2  3  4  5  7  8 
    34 16  7  4  1  1  1 


## Troubleshooting

If you see unexpected behaviour, try `read_dv(...,do_warn=TRUE)` to obtain more diagnostic information during the process of reading and parsing the DataVolley file. Also check the text encoding specified to `read_dv` (did you specify one??)


## More

For more information about DataVolley, see http://www.dataproject.com/IT/en/Volleyball.

