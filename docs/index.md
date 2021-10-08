LAB07
================
Carmen Chen
10/8/2021

if(knitr::is\_html\_output(excludes = "gfm)){

}

\#Question 1: How many sars-cov-2 papers? Build an automatic counter of
sars-cov-2 papers using PubMed. You will need to apply XPath as we did
during the lecture to extract the number of results returned by PubMed
in the following web address:

``` r
# Downloading the website
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2")

# Finding the counts
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/section[1]/div[2]/div[1]/span") #inspect -> copy full x path

# Turning it into text
counts <- as.character(counts)

# Extracting the data using regex
stringr::str_extract(counts, "[0-9,]+")
```

    ## [1] "114,592"

``` r
stringr::str_extract(counts, "[[:digit:],]+")
```

    ## [1] "114,592"

``` r
stringr::str_replace(counts, "[^[:digit:]]+([[:digit:]]+),([[:digit:]]+)[^[:digit:]]+", "\\1\\2")
```

    ## [1] "114592"

\#Question 2: Academic publications on COVID19 and Hawaii

``` r
library(httr)
query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    db = "pubmed",
    term = "covid19 hawaii",
    retmax = 1000)
)

query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/",
  path = "entrez/eutils/esearch.fcgi",
  query = list(
    db = "pubmed",
    term = "covid19 hawaii",
    retmax = 1000)
)
# Extracting the content of the response of GET
ids <- httr::content(query_ids)
```

Rscript –vanilla -e ‘rmarkdown::render(“index.Rmd”, output\_format =
“all”)’
