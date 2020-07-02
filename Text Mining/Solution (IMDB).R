install.packages("XML")
library(XML)
library(rvest)
library(syuzhet)
library(tm)

Iurl <- "https://www.imdb.com/title/tt0111161/reviews?ref_=tt_urv"
IMDB_review <- NULL
for (i in 1:12) {
  murl <- read_html(as.character(paste(Iurl,i,sep="=")))  
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_review <- c(IMDB_review,rev)
}
View(IMDB_review)
length(IMDB_review)
library(xlsx)
write.xlsx(IMDB_review, "Movie.xlsx")

head(IMDB_review)
corpus <- Corpus(VectorSource(IMDB_review))
inspect(corpus[1:2])

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:2])
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:2])
cleanset <- tm_map(corpus, removeWords, c('movie','movies'))
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:2])

tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:12]
r_sum <- rowSums(tdm)
r_sum <- subset(r_sum, r_sum>= 15)
barplot(r_sum)

na.omit(Movie)
View(Movie)
review <- as.character(Movie[-1,])
RVW <- get_nrc_sentiment(review)
head(RVW)
review[2]
barplot(colSums(RVW))



