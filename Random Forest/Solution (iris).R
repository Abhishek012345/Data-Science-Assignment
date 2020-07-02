library(MASS)
library(caret)
library(randomForest)

data(iris)
irissetosa <- iris[iris$Species=="setosa",]
irisversicolor <- iris[iris$Species=="versicolor",]
irisvirginica <- iris[iris$Species=="virginica",]
iris_train <- rbind(irissetosa[1:25,], irisversicolor[1:25,], irisvirginica[1:25,])
iris_test <- rbind(irissetosa[26:50,], irisversicolor[26:50,], irisvirginica[26:50,])

rand_frst <- randomForest(Species~., data = iris_train)
rand_frst
pred1 <- predict(rand_frst, iris_train)
head(iris_train$Species)

confusionMatrix(pred1, iris_train$Species)
pred2 <- predict(rand_frst, iris_test)
confusionMatrix(pred2, iris_test$Species)

plot(rand_frst)
rand_frst1 <- randomForest(Species~., data = iris_train, ntree= 200, importance = TRUE)
rand_frst1
pred1 <- predict(rand_frst1, iris_train)
confusionMatrix(pred1, iris_train$Species)
pred2 <- predict(rand_frst1, iris_test)
confusionMatrix(pred1, iris_test$Species)

hist(treesize(rand_frst1))
partialPlot(rand_frst1, iris_train, Petal.Length, "versicolor")




