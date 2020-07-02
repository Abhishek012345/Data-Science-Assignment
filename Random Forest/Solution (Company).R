library(MASS)
library(caret)
install.packages("randomForest")
library(randomForest)

set.seed(244)
hist(Company_Data$Sales)

High_sale <- ifelse(Company_Data$Sales < 9, "NO" , "YES")
Comp_dt <- data.frame(Company_Data[2:11],High_sale)
str(Comp_dt)
table(Comp_dt$High_sale)
trn_tst <- sample(2, nrow(Comp_dt), replace = TRUE, prob = c(0.7,0.3))
train<- Comp_dt[trn_tst==1,]
test<- Comp_dt[trn_tst==2,]
set.seed(254)

rand_frst <- randomForest(High_sale~., data = train)
rand_frst
pred1 <- predict(rand_frst, train)
head(pred1)
confusionMatrix(pred1, train$High_sale)

pred2 <- predict(rand_frst, test)
confusionMatrix(pred2, test$High_sale)

plot(rand_frst)

rand_frst1 <- randomForest(High_sale~., data = train, ntree=350, importance = TRUE)
rand_frst1

pred1 <- predict(rand_frst1, train)
confusionMatrix(pred1, train$High_sale)
pred2 <- predict(rand_frst1, test)
confusionMatrix(pred2, test$High_sale)

hist(treesize(rand_frst1))