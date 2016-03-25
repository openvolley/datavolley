# datavolley
R package for reading datavolley scouting files

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


## More

For more information about DataVolley, see http://www.dataproject.com/IT/en/Volleyball.

