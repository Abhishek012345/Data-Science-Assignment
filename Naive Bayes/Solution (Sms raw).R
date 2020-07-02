library(readr)
View(sms_raw)

sms_raw$type <- factor(sms_raw$type)
sms_raw$type
str(sms_raw)

str(sms_raw$type)
table(sms_raw$type)

install.packages("tm")
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_dict

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_test
sms_train

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}
convert_counts
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
sms_test
View(sms_test)
str(sms_test)
View(sms_train)

install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred
table(sms_test_pred)
prop.table(table(sms_test_pred))

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

install.packages("wordcloud")
library(wordcloud)
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))



wordcloud(sms_corpus_train, max.words = 50, min.freq=40, random.order = FALSE)








