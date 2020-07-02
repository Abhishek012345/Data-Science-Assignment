library(MASS)
library(caret)
library(randomForest)

set.seed(222)
hist(Fraud_check$Taxable.Income)
RS_gd <- ifelse(Fraud_check$Taxable.Income <=30000, "Risky", "Good")
frd_chk <- data.frame(Fraud_check, RS_gd)
frd_chk1 =frd_chk[,c(1:7)]
str(frd_chk1)

table(frd_chk1$RS_gd)
trn_tst <- sample(2, nrow(frd_chk1), replace = TRUE, prob = c(0.7,0.3))
train <- frd_chk[trn_tst==1,]
test <- frd_chk[trn_tst==2,]

rand_frst <- randomForest(RS_gd~., data = train)
pred1<- predict(rand_frst, train)
pred1

confusionMatrix(pred1, train$RS_gd)
pred2<- predict(rand_frst, test)
confusionMatrix(pred2, test$RS_gd)

plot(rand_frst)

rand_frst1 <- randomForest(RS_gd~., data = train, ntree= 300, importance = TRUE)
rand_frst1
pred1<- predict(rand_frst1, train)
confusionMatrix(pred1, train$RS_gd)
pred2<- predict(rand_frst1, test)
confusionMatrix(pred2, test$RS_gd)

hist(treesize(rand_frst1))
partialPlot(rand_frst1, train, Taxable.Income, "Good")

