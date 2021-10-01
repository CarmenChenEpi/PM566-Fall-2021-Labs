lab05
================
Carmen Chen
9/24/2021

\#Read in the data

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#met
if (!file.exists("../met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "../met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("../met_all.gz")

# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

``` r
met <- merge(
  x = met,
  y = stations,
  all.x = TRUE, all.y = FALSE,
  by.x = "USAFID", by.y = "USAF"
)
met %>% nrow()
```

    ## [1] 2377343

## Question 1: Representative station for the US

What is the median station in terms of temperature, wind speed, and
atmospheric pressure? Look for the three weather stations that best
represent continental US using the quantile() function. Do these three
coincide?

First, generate a representative version of each station. We will use
the averages (median could also be a good way to represent it, but it
will depend on the case)

``` r
station_averages <- met[, .(
  temp = mean(temp, na.rm = TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
), by = USAFID]
```

Now, we need to identify the quantiles per variable

``` r
medians <- station_averages[, .(
  temp_50 = quantile(temp, probs = .5, na.rm = TRUE),
  wind.sp_50 = quantile(wind.sp, probs = .5, na.rm = TRUE),
  atm.press_50 = quantile(atm.press, probs = .5, na.rm = TRUE)
)]

medians
```

    ##     temp_50 wind.sp_50 atm.press_50
    ## 1: 23.68406   2.461838     1014.691

Now we can find the stations that are the closest to these. (hint:
`which.min()`)

``` r
station_averages[, temp_dist := abs(temp - medians$temp_50)]
median_temp_station <- station_averages[order(temp_dist)][1]
median_temp_station
```

    ##    USAFID     temp  wind.sp atm.press   temp_dist
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

The median temperature station is 720458.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

We first need to recover the state variable, by MERGING.

``` r
station_averages <- merge(x = station_averages, y = stations, by.x = "USAFID", by.y = "USAF", all.x = TRUE, all.y = TRUE)
station_averages
```

    ##        USAFID temp wind.sp atm.press temp_dist CTRY STATE
    ##     1:   7018   NA      NA        NA        NA <NA>  <NA>
    ##     2:   7026   NA      NA        NA        NA   AF  <NA>
    ##     3:   7070   NA      NA        NA        NA   AF  <NA>
    ##     4:   8260   NA      NA        NA        NA <NA>  <NA>
    ##     5:   8268   NA      NA        NA        NA   AF  <NA>
    ##    ---                                                   
    ## 26143: 998760   NA      NA        NA        NA <NA>  <NA>
    ## 26144: 998860   NA      NA        NA        NA <NA>  <NA>
    ## 26145: 999100   NA      NA        NA        NA <NA>  <NA>
    ## 26146: 999110   NA      NA        NA        NA <NA>  <NA>
    ## 26147: 999120   NA      NA        NA        NA <NA>  <NA>

``` r
station_averages[, temp_50 := quantile(temp, probs = .5, na.rm = TRUE), by = STATE ]
station_averages[, wind.sp_50 := quantile(wind.sp, probs = .5, na.rm = TRUE), by = STATE ]
station_averages[, atm.press_50 := quantile(atm.press, probs = .5, na.rm = TRUE), by = STATE ]
```

Now, the euclidean distance… $\\sqrt{\\sum\_i(x\_i - y\_i)^2}$

``` r
station_averages[, eudist := sqrt(
  (temp - temp_50)^2 + (wind.sp - wind.sp_50)^2
)]
```

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use leaflet() to visualize all \~100 points in
the same figure, applying different colors for those identified in this
question.

## Question 4: Means of means

Using the quantile() function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

low: temp &lt; 20 Mid: temp &gt;= 20 and temp &lt; 25 High: temp &gt;=
25 Once you are done with that, you can compute the following:

Number of entries (records), Number of NA entries, Number of stations,
Number of states included, and Mean temperature, wind-speed, and
atmospheric pressure.

``` r
met[, state_temp := mean(temp, na.rm = TRUE), by = STATE]
met[, temp_cat := fifelse(
  state_temp < 20, "low-temp", 
  fifelse(state_temp < 25, "mid-temp", "high-temp"))
  ]
```

Let’s make sure that we don’t have NAs.

``` r
table(met$temp_cat, useNA = "always")
```

    ## 
    ## high-temp  low-temp  mid-temp      <NA> 
    ##    811126    430794   1135423         0

Now, let’s summarize

``` r
met[, .(
  N_entries = .N,
  N_stations = length(unique(USAFID))
), by = temp_cat]
```

    ##     temp_cat N_entries N_stations
    ## 1:  mid-temp   1135423        781
    ## 2: high-temp    811126        555
    ## 3:  low-temp    430794        259