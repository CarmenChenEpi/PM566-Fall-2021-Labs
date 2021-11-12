Lab10
================
Carmen Chen
11/5/2021

\#Set up

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

\#\#4.1

``` r
q <- dbSendQuery(con,"
SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)")

dbFetch(q, n = 10) #keep fetching a subset of the dataset
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
dbFetch(q, n = 10)
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16210         354        2       158   1.99 2007-01-25 23:55:37.996577
    ## 2       16240         369        2       913   7.99 2007-01-30 09:33:24.996577
    ## 3       16275         386        1       583   7.99 2007-01-28 10:17:21.996577
    ## 4       16277         387        1       697   7.99 2007-01-29 00:32:30.996577
    ## 5       16289         391        1       891   7.99 2007-01-30 06:11:38.996577
    ## 6       16302         400        2       516   1.99 2007-01-28 01:40:13.996577
    ## 7       16306         401        2       811   1.99 2007-01-29 17:59:08.996577
    ## 8       16307         402        2       801   1.99 2007-01-29 16:04:16.996577
    ## 9       16314         407        1       619   7.99 2007-01-28 14:20:52.996577
    ## 10      16320         411        2       972   1.99 2007-01-30 18:49:33.996577

``` r
dbClearResult(q) #you need to clear the result to avoid exhausting resources
```

\#\#4.2

``` r
dbGetQuery(con,"
SELECT *
FROM payment
WHERE amount > 5
LIMIT 5")
```

    ##   payment_id customer_id staff_id rental_id amount               payment_date
    ## 1      16052         269        2       678   6.99 2007-01-28 21:44:14.996577
    ## 2      16058         271        1      1096   8.99 2007-01-31 11:59:15.996577
    ## 3      16060         272        1       405   6.99 2007-01-27 12:01:05.996577
    ## 4      16061         272        1      1041   6.99 2007-01-31 04:14:49.996577
    ## 5      16068         274        1       394   5.99 2007-01-27 09:54:37.996577

Bonus: Count the number of the results

``` r
dbGetQuery(con,"
SELECT COUNT(*)
FROM payment
WHERE amount > 5")
```

    ##   COUNT(*)
    ## 1      266

Count the number of the staffs

``` r
dbGetQuery(con,"
SELECT staff_id, COUNT(*) AS N
FROM payment
/*GROUP BY goes after WHERE*/
WHERE amount > 5
GROUP BY staff_id")
```

    ##   staff_id   N
    ## 1        1 151
    ## 2        2 115

\#Question 5

``` r
dbGetQuery(con, "
SELECT payment_id, COUNT(*) AS N
FROM payment AS p
  INNER JOIN customer AS c
  on p.customer_id = c.customer_id
WHERE c.last_name = 'DAVIS'")
```

    ##   payment_id N
    ## 1      16685 3

\#Question 6

\#\#6.1

``` r
dbGetQuery(con, "
SELECT COUNT(*)
FROM rental")
```

    ##   COUNT(*)
    ## 1    16044

\#\#6.2

``` r
dbGetQuery(con, "
SELECT customer_id, COUNT(*) AS `N Rentals`
FROM rental
GROUP BY customer_id
LIMIT 5")
```

    ##   customer_id N Rentals
    ## 1           1        32
    ## 2           2        27
    ## 3           3        26
    ## 4           4        22
    ## 5           5        38

\#\#6.3

``` r
dbGetQuery(con, "
SELECT customer_id, COUNT(*) AS `N Rentals`, 1 AS Number /*assign 1 to every ovbervation in the Number column*/
FROM rental
GROUP BY customer_id
/*This is equivalent to
ORDER BY -`N Rentals`*/
ORDER BY `N Rentals` DESC /*sort*/
LIMIT 5")
```

    ##   customer_id N Rentals Number
    ## 1         148        46      1
    ## 2         526        45      1
    ## 3         236        42      1
    ## 4         144        42      1
    ## 5          75        41      1

\#\#6.4

``` r
dbGetQuery(con, "
SELECT customer_id, COUNT(*) AS `N Rentals`
FROM rental
GROUP BY customer_id
HAVING `N Rentals` >= 40
ORDER BY `N Rentals` DESC")
```

    ##   customer_id N Rentals
    ## 1         148        46
    ## 2         526        45
    ## 3         236        42
    ## 4         144        42
    ## 5          75        41
    ## 6         469        40
    ## 7         197        40

\#Question 7

``` r
dbGetQuery(con, "
SELECT 
  MAX(amount) AS `max`,
  MIN(amount) AS `min`,
  AVG(amount) AS `avg`,
  SUM(amount) AS `sum`
FROM payment
")
```

    ##     max  min      avg     sum
    ## 1 11.99 0.99 4.169775 4824.43

\#\#7.1

``` r
dbGetQuery(con, "
SELECT 
  customer_id,
  MAX(amount) AS `max`,
  MIN(amount) AS `min`,
  AVG(amount) AS `avg`,
  SUM(amount) AS `sum`
FROM payment
GROUP BY customer_id
LIMIT 10")
```

    ##    customer_id  max  min      avg   sum
    ## 1            1 2.99 0.99 1.990000  3.98
    ## 2            2 4.99 4.99 4.990000  4.99
    ## 3            3 2.99 1.99 2.490000  4.98
    ## 4            5 6.99 0.99 3.323333  9.97
    ## 5            6 4.99 0.99 2.990000  8.97
    ## 6            7 5.99 0.99 4.190000 20.95
    ## 7            8 6.99 6.99 6.990000  6.99
    ## 8            9 4.99 0.99 3.656667 10.97
    ## 9           10 4.99 4.99 4.990000  4.99
    ## 10          11 6.99 6.99 6.990000  6.99

\#\#7.2

``` r
dbGetQuery(con, "
SELECT 
  customer_id,
  MAX(amount) AS `max`,
  MIN(amount) AS `min`,
  AVG(amount) AS `avg`,
  SUM(amount) AS `sum`,
  COUNT(*) AS `N`
FROM payment
GROUP BY customer_id
HAVING COUNT(*) > 5")
```

    ##    customer_id  max  min      avg   sum N
    ## 1           19 9.99 0.99 4.490000 26.94 6
    ## 2           53 9.99 0.99 4.490000 26.94 6
    ## 3          109 7.99 0.99 3.990000 27.93 7
    ## 4          161 5.99 0.99 2.990000 17.94 6
    ## 5          197 3.99 0.99 2.615000 20.92 8
    ## 6          207 6.99 0.99 2.990000 17.94 6
    ## 7          239 7.99 2.99 5.656667 33.94 6
    ## 8          245 8.99 0.99 4.823333 28.94 6
    ## 9          251 4.99 1.99 3.323333 19.94 6
    ## 10         269 6.99 0.99 3.156667 18.94 6
    ## 11         274 5.99 2.99 4.156667 24.94 6
    ## 12         371 6.99 0.99 4.323333 25.94 6
    ## 13         506 8.99 0.99 4.132857 28.93 7
    ## 14         596 6.99 0.99 3.823333 22.94 6

\#Clean up

``` r
dbDisconnect(con)
```
