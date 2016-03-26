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
x <- read_dv(system.file("extdata/example_data.dvw",package="datavolley"))
summary(x)
```

    Match summary:
    Date: 2015-12-28 18:00:00
    League: Middle European Cup Women 2015/16 - WIL
    Teams: Kamnik (RIBIC Gašper/JEMEC Aljoša)
           vs
           Br.Maribor (ŠKORC Sebastijan/)
    Result: 2-3 (25-21, 15-25, 25-19, 23-25, 6-15)
    Duration: 113 minutes


Extract the data corresponding to serves:

```R
xs <- subset(x$plays,skill=="Serve")
nrow(xs)
```

    [1] 198


If you see unexpected behaviour, try `read_dv(...,do_warn=TRUE)` to obtain more diagnostic information during the process of reading and parsing the DataVolley file.


## More

For more information about DataVolley, see http://www.dataproject.com/IT/en/Volleyball.

