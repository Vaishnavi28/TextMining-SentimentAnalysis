#######Extracting Tweets#######
library(twitteR)
library(ROAuth)
library(tm)
library(SnowballC)
library(ggplot2)
## 0. Getting twitter Authentication:
key="hidden"
setwd="C:/Users/vaish/Dropbox/CSU/Data Mining"
secret="hidden"
download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/vaish/Dropbox/CSU/Data Mining/cacert.pem",
              method="auto")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
options(httr_oauth_cache=T)

# Set up call back URL to http://127.0.0.1:1410/ in twitter app and
#set access token and access secret to do local verification instead of browser based verification
setup_twitter_oauth(key, secret,access_token=NULL,access_secret=NULL)
save(authenticate, file="twitter authentication.Rdata")

## 1: retrieve tweets from Twitter
library(twitteR)
tweets <- userTimeline("RDataMining", n = 3200)

## 2: download @RDataMining tweets from RDataMining.com
url <- "http://www.rdatamining.com/data/rdmTweets-201306.RData"
download.file(url, destfile = "C:/Users/vaish/Dropbox/CSU/Data Mining/rdmTweets-201703.RData")

## 3.load tweets into R
load(file = "C:/Users/vaish/Dropbox/CSU/Data Mining/rdmTweets-201703.RData")
(n.tweet <- length(tweets))

#4. convert tweets to a data frame
tweets.df <- twListToDF(tweets)
dim(tweets.df)

for (i in c(1:2, 320)) {
cat(paste0("[", i, "] "))
writeLines(strwrap(tweets.df$text[i], 60))}

#5. build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))

#6. convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# myCorpus <- tm_map(myCorpus, tolower)

#7. remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#8.remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
# remove punctuation
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#9. Data Cleaning
#add extra stop words: "available" and "via" and "through"
myStopwords <- c(stopwords('english'), "available", "via","through")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

#10.keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# take the stem words and store it
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first10 tweets
inspect(myCorpus[1:10])

#11.The code below is used for to make text fit for paper width
for (i in c(1:2, 320)) {
cat(paste0("[", i, "] "))
writeLines(strwrap(as.character(myCorpus[[i]]), 60))
}
#Stem completion
stemCompletion2 <- function(x, dictionary) {
x <- unlist(strsplit(as.character(x), " "))
# Unexpectedly, stemCompletion completes an empty string to
# a word in dictionary. Remove empty string to avoid above issue.
# http://stackoverflow.com/questions/25206049/stemcompletion-is-not-working
x <- x[x != ""]
x <- stemCompletion(x, dictionary=dictionary)
x <- paste(x, sep="", collapse=" ")
PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))

#12. count frequency of "mining"
miningCases <- lapply(myCorpusCopy,
                      function(x) { grep(as.character(x), pattern = "mining")} )
sum(unlist(miningCases))
## 90
# count frequency of "miner"
minerCases <- lapply(myCorpusCopy,
                     function(x) {grep(as.character(x), pattern = "miner")} )
sum(unlist(minerCases))
##  8
# replace "miner" with "mining"
myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                   pattern = "miner", replacement = "mining")
#Document matrix:
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
tdm

##13.Frequent words and association:
idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx + (0:5), 101:110])
#inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))
#putting in a matrix form:
term.freq <- rowSums(as.matrix(tdm))
#frequency greater than 15
term.freq <- subset(term.freq, term.freq >= 15)
#putting it in a data frame
df <- data.frame(term = names(term.freq), freq = term.freq)
df
#plotting the frequencies
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

#14. Association Mining
# which words are associated with 'r'?
findAssocs(tdm, "r", 0.2)
# which words are associated with 'mining'?
findAssocs(tdm, "mining", 0.25)

#PLotting using rgraphviz from bioconductors, since its removed from CRAN
# http://stackoverflow.com/questions/18023300/is-rgraphviz-no-longer-available-for-r
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(graph)
library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T, cex=1)


##15.Word Cloud:
library(wordcloud)
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 4,

                    random.order = F, colors = pal)
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)

###16. cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 6) # cut tree into 6 clusters

###17.Applying kmeans clustering
m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122)
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k) {
cat(paste("cluster ", i, ": ", sep = ""))
s <- sort(kmeansResult$centers[i, ], decreasing = T)
cat(names(s)[1:5], "nn")
# print the tweets of every cluster
# print(tweets[which(kmeansResult$cluster==i)])
}

##18.Topic Modelling
dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 10) # find 10 topics
(term <- terms(lda, 8)) # first 8 terms of every topic


# first topic identified for every document (tweet)
library(data.table)
library(xts)
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
topic <- topics(lda, 1)
topics <- data.frame(date=as.IDate(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")
