library(randomForest)
library(MASS)
install.packages("gbm")
library(gbm)
data("Boston")
attach(Boston)

head(Boston)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test = Boston[-train,]
Bag = randomForest(medv~.,data = Boston, subset = train, mtry = 13)
Bag

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test =Boston[-train,]
Bag = randomForest(medv~.,data = Boston, subset = train, mtry = 13)
plot(Bag)

yhat = predict(Bag, newdata = test)
y = Boston[-train, "medv"]
plot(y,yhat)
abline(0,1,col=2)
mean((y-yhat) ^2)

importance(Bag)
Bag = randomForest(formula = medv~., data = Boston, mtry = 13, 
                   ntree = 25, subset = train)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test =Boston[-train,]
Random = randomForest(medv~.,data = Boston, subset = train, mtry = 6)
Random
plot(Random)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test =Boston[-train,]
Random = randomForest(medv~.,data = Boston, subset = train, mtry = 6)
yhat = predict(Random, newdata = test)
y = Boston[-train, "medv"]
plot(y,yhat)
abline(0,1,col=2)

mean((y-yhat)^2)
importance(Random)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test = Boston[-train,]
boost.boston = gbm(medv~., data = Boston[train,], distribution ="gaussian",
                   n.trees = 4000, interaction.depth = 4)
summary(boost.boston)

plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 4000)
mean((yhat.boost - Boston[-train, 14])^2)






