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
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
  geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

How to push the file in Terminal using command lines: git add
lab06/README git status git commit -a -m “Starting with lab06” git push

How to remove the file on track: git rm –cache -r lab06/README\_cache
