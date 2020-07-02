install.packages("naivebayes")
library(naivebayes)
library(ggplot2)
library(e1071)
salary_train <- read.csv(file.choose())

View(salary_train)
str(salary_train)
salary_train$educationno <- as.factor(salary_train$educationno)
View(salary_train$educationno)

salary_test <- read.csv(file.choose())
View(salary_test)

salary_test$educationno <- as.factor(salary_test$educationno)
par(mar=rep(2,4))
plot(salary_train$workclass[1:30], salary_train$Salary[1:30])

ggplot(data = salary_train,aes(x=Salary, y=age, fill=Salary))+ geom_boxplot() + ggtitle("Box Plot on Trained Salary Dataset")

Model <- naiveBayes(salary_train$Salary ~.,data = salary_train)
Model

Model_pred <- predict(Model, salary_test)
mean(Model_pred == salary_test$Salary)
library(caret)
confusionMatrix(Model_pred,salary_test$Salary)

ggplot(data = salary_train, aes(x=age, fill = Salary)) + geom_density(alpha = 0.7, color = 'black')


