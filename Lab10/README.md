Lab10
================
Carmen Chen
11/5/2021

**Set up**

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)
```

Are the tables there?

``` r
dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

You can also use knitr + SQL!

``` sql
PRAGMA table_info(actor)
```

``` r
x1
```

    ##   cid        name    type notnull dflt_value pk
    ## 1   0    actor_id INTEGER       0         NA  0
    ## 2   1  first_name    TEXT       0         NA  0
    ## 3   2   last_name    TEXT       0         NA  0
    ## 4   3 last_update    TEXT       0         NA  0

This is equivalent to use`dbGetQuery`

``` r
dbGetQuery(con, "PRAGMA table_info(actor)")
```

    ##   cid        name    type notnull dflt_value pk
    ## 1   0    actor_id INTEGER       0         NA  0
    ## 2   1  first_name    TEXT       0         NA  0
    ## 3   2   last_name    TEXT       0         NA  0
    ## 4   3 last_update    TEXT       0         NA  0

\#Question 1

AND using the LIMIT clause (`head()` in R) to just look at the first 5
rows

``` r
dbGetQuery(con, "
/*You can add comments*/
SELECT actor_id, first_name, last_name
FROM actor
ORDER by last_name, first_name
LIMIT 5")
```

    ##   actor_id first_name last_name
    ## 1       58  CHRISTIAN    AKROYD
    ## 2      182     DEBBIE    AKROYD
    ## 3       92    KIRSTEN    AKROYD
    ## 4      118       CUBA     ALLEN
    ## 5      145        KIM     ALLEN

\#Question 2

``` r
dbGetQuery(con, "
SELECT actor_id, first_name, last_name
FROM actor
WHERE last_name IN ('WILLIAMS', 'DAVIS')")
```

    ##   actor_id first_name last_name
    ## 1        4   JENNIFER     DAVIS
    ## 2       72       SEAN  WILLIAMS
    ## 3      101      SUSAN     DAVIS
    ## 4      110      SUSAN     DAVIS
    ## 5      137     MORGAN  WILLIAMS
    ## 6      172    GROUCHO  WILLIAMS

``` r
dbDisconnect(con)
```
