View(glass)

install.packages('caTools')  
install.packages('ggplot2')  
install.packages('class')    
install.packages('caret')    

Std.feat <-scale(glass[,1:9])
data <- cbind(Std.feat,glass[10])
anyNA(data)

head(data)

set.seed(110)
samp <- sample.split(data$Type, SplitRatio = 0.70)

train <- subset(data,samp == TRUE)

test <- subset(data,samp == FALSE)

library(class)
pred.type <- knn(train[1:9],test[1:9],train$Type, k=1)

error <- mean(pred.type!=test$Type)
library(caret)
confusionMatrix(pred.type, test)

pred.type <- NULL

error.rate <- NULL

for (i in 1:10) {
  pred.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(pred.type!=test$Type)
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))

library(ggplot2)
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)

error <- mean(predicted.type!=test$Type)

confusionMatrix(predicted.type,test$Type)





