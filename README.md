# TextMining-SentimentAnalysis

In this project tweets from twitter are extracted for user timeline "RDataMining”( downloaded @RDataMining tweets from RDataMining.com) to learn the pattern and sentiment of the tweets and to segment data based on the category.

•	Get Twitter authentication and extract data from Twitter

•	Clean extracted data and build a document-term matrix

•	Find frequent words and associations

•	Create a word cloud to visualize important words

•	Text clustering & categorization

•	Entity Extraction & Sentiment Analysis

•	Topic modelling

## Twitter Authentication:

key="hidden"
setwd="C:/Users/vaish/Dropbox/CSU/Data Mining"
secret="hidden "
download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/vaish/Dropbox/CSU/Data Mining/cacert.pem",
              method="auto")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
options(httr_oauth_cache=T)

/*Set up call back URL to http://127.0.0.1:1410/ in twitter app and set access token and access secret to do local verification instead of browser based verification*/

setup_twitter_oauth(key, secret,access_token=NULL,access_secret=NULL)

save(authenticate, file="twitter authentication.Rdata")

> load(file = "C:/Users/vaish/Dropbox/CSU/Data Mining/rdmTweets-201703.RData)
> (n.tweet <- length(tweets))
[1] 320

> tweets.df <- twListToDF(tweets)
> dim(tweets.df)
[1] 320  14

## Data Retrieval & Preprocessing:

### 1: retrieve tweets from Twitter
library(twitteR)
tweets <- userTimeline("RDataMining", n = 3200)

### 2: download @RDataMining tweets from RDataMining.com
url <- "http://www.rdatamining.com/data/rdmTweets-201306.RData"
download.file(url, destfile = "C:/Users/vaish/Dropbox/CSU/Data Mining/rdmTweets-201703.RData")

### 3.load tweets into R
load(file = "C:/Users/vaish/Dropbox/CSU/Data Mining/rdmTweets-201703.RData")
(n.tweet <- length(tweets))

### 4. convert tweets to a data frame

tweets.df <- twListToDF(tweets)
dim(tweets.df)

for (i in c(1:2, 320)) {
cat(paste0("[", i, "] "))
writeLines(strwrap(tweets.df$text[i], 60))}

### 5. build a corpus, and specify the source to be character vectors

myCorpus <- Corpus(VectorSource(tweets.df$text))

### 6. convert to lower case

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

### 7. remove URLs

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

### 8.remove anything other than English letters or space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

#### remove punctuation
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

### 9. Data Cleaning

/*add extra stop words: "available" and "via" and "through"*/

myStopwords <- c(stopwords('english'), "available", "via","through")

#remove "r" and "big" from stopwords

myStopwords <- setdiff(myStopwords, c("r", "big"))

#remove stopwords from corpus

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#remove extra whitespace

myCorpus <- tm_map(myCorpus, stripWhitespace)

### 10.keep a copy of corpus to use later as a dictionary for stem completion

myCorpusCopy <- myCorpus

#take the stem words and store it

myCorpus <- tm_map(myCorpus, stemDocument)

#inspect the first10 tweets

inspect(myCorpus[1:10])

### 11.The code below is used for to make text fit for paper width

for (i in c(1:2, 320)) {

cat(paste0("[", i, "] "))

writeLines(strwrap(as.character(myCorpus[[i]]), 60))

}

### 12.Count frequency of "mining"
miningCases <- lapply(myCorpusCopy,function(x) { grep(as.character(x), pattern = "mining")} )
sum(unlist(miningCases))
#90 
### count frequency of "miner"
minerCases <- lapply(myCorpusCopy,
                     function(x) {grep(as.character(x), pattern = "miner")} )
sum(unlist(minerCases))
#8
#replace "miner" with "mining"
myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                   pattern = "miner", replacement = "mining")
### Document matrix:
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))

### Inspect first 10 tweets:
inspect(myCorpus[1:10])

<<SimpleCorpus>>

Metadata:  corpus specific: 1, document level (indexed): 0

Content:  documents: 5

[1] exampl call java code r                                                                   
[2] simul mapreduc r big data analysi use flight data rblogger                                
[3] job opportun senior analyst big data wesfarm industri amp safeti sydney area australia job
[4] clavin open sourc softwar packag document geotag geopars                                  
[5] onlin book natur languag process python 
[6] tip r program provid biostat wiki                                                         
[7] introduct r data mine onehour video revolut analyt                                        
[8] geonam geograph databas geocod countri citi suburb place postcod                          
[9] result kdnugget annual softwar poll softwar use past month real project kdnugget          
[10] big geodata visualis


### 12. count frequency of "mining"
miningCases <- lapply(myCorpusCopy,
                      function(x) { grep(as.character(x), pattern = "mining")} )
sum(unlist(miningCases))
#90

### count frequency of "miner"
minerCases <- lapply(myCorpusCopy,
                     function(x) {grep(as.character(x), pattern = "miner")} )
sum(unlist(minerCases))
#8

<<TermDocumentMatrix (terms: 1186, documents: 320)>>
Non-/sparse entries: 11704/367816
Sparsity           : 97%
Maximal term length: 27
Weighting          : term frequency (tf)

> inspect(tdm[idx + (0:5), 101:110])
<<TermDocumentMatrix (terms: 6, documents: 10)>>
Non-/sparse entries: 44/16
Sparsity           : 27%
Maximal term length: 13
Weighting          : term frequency (tf)
Sample             :
               
                              Docs
               
  Terms           101 102 103 104 105 106 107 108 109 110


  r               0   1   1   0   0   0   0   0   1   1
  
  sec             1   1   1   1   1   1   1   1   1   1
  
  wday            1   1   1   1   1   1   1   1   1   1
  
  yday            1   1   1   1   1   1   1   1   1   1
  
  year            1   1   1   1   1   1   1   1   1   1


### To find frequent terms:

> (freq.terms <- findFreqTerms(tdm, lowfreq = 15))

 [1] "0"             "117"           "18"            "2"            
 [5] "26"            "6"             "76"            "author"       
 [9] "character"     "code"          "content"       "datetimestamp"
[13] "description"   "example"       "heading"       "hour"         
[17] "id"            "isdst"         "language"      "list"         
[21] "mday"          "meta"          "min"           "mon"          
[25] "origin"        "r"             "sec"           "wday"         
[29] "yday"          "year"          "analysis"      "big"          
[33] "data"          "use"           "package"       "book"         
[37] "introduction"  "mining"        "network"       "slides"       
[41] "19"            "social"        "see"           "computational"
[45] "group"         "application"   "21"            "research"     
[49] "position"      "22"            "tutorial"      "university"   
[53] "23"            "24"            "25"            "27"           
[57] "28"            "29"            "30"            "31"           
[61] "32"            "33"            "34"            "35"           
[65] "36"            "37"  

### Frequency of the terms greater than 15:

term.freq <- subset(term.freq, term.freq >= 15)

df <- data.frame(term = names(term.freq), freq = term.freq)

term 			                  freq
0                         0 2240
117                     117  320
18                       18  335
2                         2  640
26                       26  332
6                         6  320
76                       76  320
author               author  320
character         character 1920
code                   code   17
content             content  320
datetimestamp datetimestamp  320
description     description  320
example             example   41
heading             heading  320
hour                   hour  320
id                       id  321
isdst                 isdst  320
language           language  323
list                   list  965
mday                   mday  320
meta                   meta  320
min                     min  320
mon                     mon  320
origin               origin  320
r                         r  157
sec                     sec  320
wday                   wday  320


![alt tag](https://raw.githubusercontent.com/username/projectname/branch/path/to/img.png)
