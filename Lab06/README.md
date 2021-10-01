Lab 06
================
Carmen Chen
10/1/2021

\#\#Download the data

``` r
fn <- "mtsamples.csv"
if(!file.exists(fn))
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv", destfile = fn)

mtsamples <- read.csv(fn) #read data from a data frame
mtsamples <- as_tibble(mtsamples)
```

\#Question 1: What specialties do we have? We can use count() from dplyr
to figure out how many different catagories do we have? Are these
catagories related? overlapping? evenly distributed?

``` r
specialties <- mtsamples %>%
  count(medical_specialty)

specialties %>%
  arrange(desc(n)) %>%
  top_n(15) %>%
  knitr::kable()
```

    ## Selecting by n

| medical\_specialty            |    n |
|:------------------------------|-----:|
| Surgery                       | 1103 |
| Consult - History and Phy.    |  516 |
| Cardiovascular / Pulmonary    |  372 |
| Orthopedic                    |  355 |
| Radiology                     |  273 |
| General Medicine              |  259 |
| Gastroenterology              |  230 |
| Neurology                     |  223 |
| SOAP / Chart / Progress Notes |  166 |
| Obstetrics / Gynecology       |  160 |
| Urology                       |  158 |
| Discharge Summary             |  108 |
| ENT - Otolaryngology          |   98 |
| Neurosurgery                  |   94 |
| Hematology - Oncology         |   90 |

There are 40 specialties. Let’s take a look at the distribution:

``` r
#Method 1 (not that pretty)
ggplot(mtsamples, aes(x = medical_specialty)) +
  geom_histogram(stat = "count") +
  coord_flip() #for better reading the categories
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](README_files/figure-gfm/dist1-1.png)<!-- -->

``` r
#Method 2
ggplot(specialties, aes(x = n, y = fct_reorder(medical_specialty, n))) + #x is frequency, y is the label, sorting the medical_specialty according to n
  geom_col() #plot column
```

![](README_files/figure-gfm/dist2-1.png)<!-- -->

These are not evenly distributed.

\#Question 2 Tokenize the the words in the transcription column Count
the number of times each token appears Visualize the top 20 most
frequent words

``` r
mtsamples %>%
  unnest_tokens(output = word, input = transcription) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
  geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/token-transcript-1.png)<!-- -->

The word “patient” seems to be important, but we observe a lot of stop
words.

\#Question 3 Redo visualization but remove stopwords before Bonus points
if you remove numbers as well What do we see know that we have removed
stop words? Does it give us a better idea of what the text is about?

``` r
mtsamples %>%
  unnest_tokens(output = word, input = transcription) %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  #using regular expressions to remove numbers
  filter(!grepl("^[0-9]+$", x = word)) %>% #"grepl" tells logical vector, "^" is the beginning of the word, "$" is the end of the word, "+" tells it should be repeated at least once
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
  geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

\#Question 4 repeat question 2, but this time tokenize into bi-grams.
how does the result change if you look at tri-grams?

``` r
library(Rcpp)
```

    ## Warning: package 'Rcpp' was built under R version 4.1.1

``` r
mtsamples %>%
  unnest_ngrams(output = bigram, input = transcription, n = 2) %>%
  count(bigram, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(bigram, n))) +
  geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/bi-grams-transcript-1.png)<!-- -->

``` r
mtsamples %>%
  unnest_ngrams(output = trigram, input = transcription, n = 3) %>%
  count(trigram, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(trigram, n))) +
  geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/tri-grams-transcript-1.png)<!-- -->

Now some phrases start to show up, e.g., “tolerated the procedure”,
“prepped and draped.”

\#Question 5 Using the results you got from questions 4. Pick a word and
count the words that appears after and before it.

``` r
bigrams <- mtsamples %>%
  unnest_ngrams(output = bigram, input = transcription, n = 2) %>%
  separate(bigram, into = c("w1", "w2")) %>%
  filter((w1 == "history") | (w2 == "history"))
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 31317 rows [600,
    ## 601, 738, 739, 981, 982, 984, 985, 991, 992, 1314, 1315, 1491, 1492, 2076, 2077,
    ## 2835, 2836, 3853, 3854, ...].

``` r
bigrams %>%
  filter(w1 == "history") %>%
  select(w1, w2) %>%
  count(w2, sort = TRUE)
```

    ## # A tibble: 366 x 2
    ##    w2                  n
    ##    <chr>           <int>
    ##  1 of               4537
    ##  2 the               761
    ##  3 she               279
    ##  4 he                227
    ##  5 significant       200
    ##  6 this              200
    ##  7 and               197
    ##  8 1                 181
    ##  9 is                172
    ## 10 noncontributory   121
    ## # ... with 356 more rows

``` r
bigrams %>%
  filter(w2 == "history") %>%
  select(w1, w2) %>%
  count(w2, sort = TRUE) 
```

    ## # A tibble: 1 x 2
    ##   w2          n
    ##   <chr>   <int>
    ## 1 history  9078

Since we are looking at single words agian, it is a good idea to treat
these as singe token. So let’s remove the stop words and numbers.

``` r
bigrams %>%
  filter(w1 == "history") %>% #keeping rows with history of first word
  filter(!(w2 %in% stop_words$word) & !grepl("^[0-9]+$", w2)) %>% #do not include words with stop words or numbers
  count(w2, sort = TRUE) %>%
  top_n(10) %>%
  knitr::kable() 
```

    ## Selecting by n

| w2              |   n |
|:----------------|----:|
| significant     | 200 |
| noncontributory | 121 |
| patient         | 106 |
| negative        |  96 |
| positive        |  87 |
| unremarkable    |  53 |
| hypertension    |  50 |
| includes        |  47 |
| mother          |  43 |
| history         |  42 |

``` r
bigrams %>%
  filter(w2 == "history") %>% #keeping rows with history of first word
  filter(!(w1 %in% stop_words$word) & !grepl("^[0-9]+$", w1)) %>% #do not include words with stop words or numbers
  count(w1, sort = TRUE) %>%
  top_n(10) %>%
  knitr::kable() 
```

    ## Selecting by n

| w1          |    n |
|:------------|-----:|
| medical     | 1223 |
| family      |  941 |
| social      |  865 |
| surgical    |  491 |
| pain        |   98 |
| psychiatric |   90 |
| prior       |   87 |
| past        |   80 |
| previous    |   72 |
| personal    |   55 |

\#Question 6 Which words are most used in each of the specialties. you
can use group\_by() and top\_n() from dplyr to have the calculations be
done within each specialty. Remember to remove stopwords. How about the
most 5 used words?

How to push the file in Terminal using command lines:

git add lab06/README\* git status git commit -a -m “Starting with lab06”
git push

How to remove the file on track:

git rm –cache -r lab06/README\_cache
