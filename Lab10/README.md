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

\#Question 3

``` r
dbGetQuery(con, "PRAGMA table_info(rental)")
```

    ##   cid         name    type notnull dflt_value pk
    ## 1   0    rental_id INTEGER       0         NA  0
    ## 2   1  rental_date    TEXT       0         NA  0
    ## 3   2 inventory_id INTEGER       0         NA  0
    ## 4   3  customer_id INTEGER       0         NA  0
    ## 5   4  return_date    TEXT       0         NA  0
    ## 6   5     staff_id INTEGER       0         NA  0
    ## 7   6  last_update    TEXT       0         NA  0

``` r
dbGetQuery(con, "
SELECT DISTINCT customer_id
FROM rental
WHERE date(rental_date) = '2005-07-05'")
```

    ##    customer_id
    ## 1          565
    ## 2          242
    ## 3           37
    ## 4           60
    ## 5          594
    ## 6            8
    ## 7          490
    ## 8          476
    ## 9          322
    ## 10         298
    ## 11         382
    ## 12         138
    ## 13         520
    ## 14         536
    ## 15         114
    ## 16         111
    ## 17         296
    ## 18         586
    ## 19         349
    ## 20         397
    ## 21         369
    ## 22         421
    ## 23         142
    ## 24         169
    ## 25         348
    ## 26         553
    ## 27         295

\#Question 4

``` r
dbGetQuery(con, "PRAGMA table_info(payment)")
```

    ##   cid         name    type notnull dflt_value pk
    ## 1   0   payment_id INTEGER       0         NA  0
    ## 2   1  customer_id INTEGER       0         NA  0
    ## 3   2     staff_id INTEGER       0         NA  0
    ## 4   3    rental_id INTEGER       0         NA  0
    ## 5   4       amount    REAL       0         NA  0
    ## 6   5 payment_date    TEXT       0         NA  0

``` r
q <- dbSendQuery(con,"
SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)")

dbFetch(q, n = 10)
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16050         269        2         7   1.99 2007-01-24 21:40:19.996577
    ## 2       16056         270        1       193   1.99 2007-01-26 05:10:14.996577
    ## 3       16081         282        2        48   1.99 2007-01-25 04:49:12.996577
    ## 4       16103         294        1       595   1.99 2007-01-28 12:28:20.996577
    ## 5       16133         307        1       614   1.99 2007-01-28 14:01:54.996577
    ## 6       16158         316        1      1065   1.99 2007-01-31 07:23:22.996577
    ## 7       16160         318        1       224   9.99 2007-01-26 08:46:53.996577
    ## 8       16161         319        1        15   9.99 2007-01-24 23:07:48.996577
    ## 9       16180         330        2       967   7.99 2007-01-30 17:40:32.996577
    ## 10      16206         351        1      1137   1.99 2007-01-31 17:48:40.996577

``` r
dbDisconnect(con)
```

    ## Warning in connection_release(conn@ptr): There are 1 result in use. The
    ## connection will be released when they are closed
