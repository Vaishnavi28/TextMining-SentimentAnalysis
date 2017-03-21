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

### Word Frequencies:
![alt tag](https://github.com/Vaishnavi28/TextMining-SentimentAnalysis/blob/master/Rplot-WordFrequencies.png)

## Association Mining:

### which words are associated with 'r'?

> findAssocs(tdm, "r", 0.2)

example    code 
   0.33    0.29 
   
### which words are associated with 'mining'?

> findAssocs(tdm, "mining", 0.25)

##data 		0.48

##mahout 		0.30

##recommendation 	0.30

##sets 		0.30

##supports 		0.30

##frequent 		0.27

##itemset 		0.26

##PLotting using rgraphviz from bioconductors, since its removed from CRAN http://stackoverflow.com/questions/18023300/is-rgraphviz-no-longer-available-for-r

source("http://bioconductor.org/biocLite.R")

biocLite("Rgraphviz")

library(graph)

library(Rgraphviz)

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T, cex=1)

#### Association Plot:

![alt tag](https://github.com/Vaishnavi28/TextMining-SentimentAnalysis/blob/master/Rplot-AssociationGraph.png)

## Word Cloud:

library(wordcloud)
m <- as.matrix(tdm)
### calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
#colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
### plot word cloud
Plot words with minimum frequency of 4:
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 4,

                    random.order = F, colors = pal)
#remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)

#### Word Cloud:

![alt tag](https://github.com/Vaishnavi28/TextMining-SentimentAnalysis/blob/master/Rplot-WordCloud.png)

•	Character, 0 and list are the centre of word cloud with biggest font. 
•	Database, research,content,author, hour, year, day, application, knowledge, data are second largest in the word cloud
•	While it is also seen that ‘mining’, ‘r’ that are highlighted in red are also part of the word cloud with slightly less populated.

## Clustering:

### Cluster Centre using kmeans:

m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122)
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

  description example heading hour    id isdst language  list mday meta
  
1           1   0.030       1    1 1.000     1    1.015 3.015    1    1

2           1   0.231       1    1 1.000     1    1.000 3.077    1    1

3           1   0.095       1    1 1.000     1    1.048 3.000    1    1

4           1   0.065       1    1 1.013     1    1.013 3.000    1    1

5           1   0.274       1    1 1.000     1    1.000 3.012    1    1

6           1   0.000       1    1 1.000     1    1.000 3.000    1    1

  min mon origin     r sec wday yday year analysis   big  data   use
  
1   1   1      1 0.197   1    1    1    1    0.136 0.136 1.015 0.015

2   1   1      1 0.974   1    1    1    1    0.026 0.154 1.487 0.231

3   1   1      1 0.286   1    1    1    1    0.857 0.000 0.048 0.095

4   1   1      1 0.000   1    1    1    1    0.078 0.013 0.000 0.052

5   1   1      1 1.190   1    1    1    1    0.083 0.000 0.024 0.143

6   1   1      1 0.000   1    1    1    1    0.091 0.152 0.515 0.000


  package  book mining network slides social computational application
  
1   0.015 0.015  0.409   0.000  0.091  0.000         0.061       0.076

2   0.205 0.256  1.128   0.000  0.051  0.000         0.026       0.154

3   0.095 0.000  0.095   0.952  0.095  0.810         0.000       0.000

4   0.117 0.052  0.104   0.013  0.104  0.013         0.052       0.026

5   0.190 0.048  0.107   0.036  0.167  0.000         0.107       0.024

6   0.000 0.000  0.091   0.000  0.000  0.121         0.000       0.061


  research position tutorial university    27    28    29    30
  
1    0.030    0.076    0.076      0.030 0.045 0.015 0.061 0.030

2    0.026    0.000    0.000      0.000 0.051 0.103 0.051 0.051

3    0.048    0.190    0.190      0.048 0.095 0.000 0.238 0.238

4    0.013    0.039    0.052      0.091 0.013 0.052 0.000 0.039

5    0.000    0.000    0.095      0.000 0.071 0.060 0.071 0.036

6    0.970    0.667    0.000      0.424 0.091 0.091 0.030 0.091

0 117    18 2    26 6 76 author character content datetimestamp

1 7   1 1.045 2 1.015 1  1      1         6       1             1

2 7   1 1.051 2 1.026 1  1      1         6       1             1

3 7   1 1.048 2 1.000 1  1      1         6       1             1

4 7   1 1.104 2 1.052 1  1      1         6       1             1

5 7   1 1.012 2 1.036 1  1      1         6       1             1

6 7   1 1.000 2 1.091 1  1      1         6       1             1

### Cluster Dendrogram:
##cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6) # cut tree into 6 clusters.

#### Cluster Dendrogram:

![alt tag](https://github.com/Vaishnavi28/TextMining-SentimentAnalysis/blob/master/Rplot-ClusterDendogram.png)

#### Print the tweet of every Cluster:

for (i in 1:k) {
+ cat(paste("cluster ", i, ": ", sep = ""))
+ s <- sort(kmeansResult$centers[i, ], decreasing = T)
+ cat(names(s)[1:5], "nn")

#No of Clusters=6

cluster 1: 0 character 
cluster 2: analysis network social research network book application tutorial
cluster 3: r data mining
cluster 4: language id year yday wday mon mday meta hour heading datetime
cluster 5: list 2 
cluster 6: list 2

Data seems to be clear and relevantly segmented within each cluster.

## Topic Modelling:

This is to find first topic identified in every tweet.

> lda <- LDA(dtm, k = 8) # find 8 topics
> (term <- terms(lda, 6)) # first 6 terms of every topic

> term <- apply(term, MARGIN = 2, paste, collapse = ", ")
> topic <- topics(lda, 1)
> topics <- data.frame(date=as.IDate(tweets.df$created), topic)

> topics <- data.frame(date=as.IDate(tweets.df$created), topic)
> qplot(date, ..count.., data=topics, geom="density",
+       fill=term[topic], position="stack")

#### Topic Modelling:

![alt tag](https://github.com/Vaishnavi28/TextMining-SentimentAnalysis/blob/master/R-TopicModelling.png)

Based on date in the x-axis and count in the y- axis, topics that are covered in every tweet are identified as shown above.

## Inference:

To conclude, the sentiment of the tweets is analyzed and is found that, it’s been more 
confined with r, code, mining, computation, character, dates, package, analytics, university,
scientist, research, position etc. This shows that the sentiment of people for user timeline in 
Rdatamining relevantly hits the topic that is more confined with process of mining and days 
they were tweeted about university, slides, materials, examples, clusters. 
The overall sentiment of people’s mind seems to be positive and are involved to learn and 
understand Data Analytics and Mining.





