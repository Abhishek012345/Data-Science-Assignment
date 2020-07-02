install.packages("rvest")
install.packages("xml")
install.packages("magrittr")
library(rvest)
library(XML)
library(magrittr)

amz_url <- "https://www.amazon.in/Apple-iPhone-Xs-Max-64GB/dp/B07J3CJM4N/ref=sr_1_1?crid=DJ8J0PBXF60A&dchild=1&keywords=iphone+11+pro+64gb&qid=1589378770&sprefix=iphone+11%2Caps%2C1044&sr=8-1"
amz_revw <- NULL
for (i in 1:12) {
  murl <- read_html(as.character(paste(amz_url,i,sep="=")))  
  murl
  rev <- murl %>%
  html_nodes(".review-text") %>%
  html_text()
  amz_revw <- c(amz_revw,rev)
  View (amz_revw)
}

library(tm)
install.packages("xlsx")
library(xlsx)
write.xlsx(amz_revw, "amazon.xlsx")

View(amazon)

corpus <- amazon$x
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:10])
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:2])
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:2])
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:2])
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:5,1:15]
r_sum <- rowSums(tdm)
r_sum <- subset(r_sum, r_sum>= 20)
barplot(r_sum)

library(syuzhet)
amz_sntmt <- get_nrc_sentiment(amazon$x)
head(amz_sntmt)
barplot(colSums(amz_sntmt), main = "Sentiment Score for Amazon Extracted Reviews")





