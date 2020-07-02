install.packages("twitteR")
install.packages("ROAuth")
library(ROAuth)
library(twitteR)

cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', 
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', 
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

install.packages("base64enc")
library(base64enc)
install.packages("httpuv")
library(httpuv)

twitteR:::setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", 
                              "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", 
                              "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh", 
                              "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")
Tweets <-userTimeline('urstrulyMahesh', n=3200,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
library(tm)
corpus <- TweetsDF$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:10])

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:10])
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:10])
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
clean <- tm_map(corpus, removeWords, stopwords('english'))
rmv_url <- function(x)gsub('http[[:alnum:]]*','',x)
cln_st <- tm_map(clean, content_transformer(rmv_url))

inspect(cln_st[1:10])

cln_st <- tm_map(cln_st, removeWords, c('Tweets'))
cln_st <- tm_map(cln_st, gsub, pattern = 'pages', replacement ='page')
inspect(cln_st[1:5])
cln_st <- tm_map(cln_st, stripWhitespace)
inspect(cln_st[1:5])

term_dm <- TermDocumentMatrix(cln_st)
term_dm
term_dm <- as.matrix(term_dm)
term_dm[1:12, 1:15]
Br_plt <- rowSums(term_dm)
Br_plt <- subset(Br_plt, Br_plt >= 15)
barplot(Br_plt)
install.packages("tmap")
library(wordcloud)
Br_plt <- sort(rowSums(term_dm),decreasing = FALSE)

install.packages("syuzhet")
tweet_data <-as.character(TweetsDF$text)
class(tweet_data)
library(syuzhet)
sntmnt <- get_nrc_sentiment(tweet_data)
head(sntmnt)
get_nrc_sentiment('pretending')
barplot(colSums(sntmnt), las = 2.5, col = 'black',
        ylab = 'Count',main= 'Sentiment scores for Twitter')










