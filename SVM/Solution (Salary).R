library(kernlab)
library(caret)
library(ggplot2)
str(Salary_train)
Salary_train$educationno <- as.factor(Salary_train$educationno)
str(Salary_Test)
Salary_Test$educationno <- as.factor(Salary_Test$educationno)

plot(Salary_train$workclass, Salary_train$Salary)

model1 <- ksvm(Salary_train$Salary~., data = Salary_train, kernel = "vanilladot")
model1

Sal_pred <- predict(model1, Salary_Test)
table(Sal_pred, Salary_Test$Salary)
Condt <- Sal_pred == Salary_Test$Salary
table(Condt)
prop.table(table(Condt))

######## Applying POLYDOT Kernel ########## ##########
model2 <- ksvm(Salary_train$Salary~., data = Salary_train, kernel = "polydot")
model2

Sal_pred2 <- predict(model2, Salary_Test)
table(Sal_pred2, Salary_Test$Salary)
Condt2 <- Sal_pred2 == Salary_Test$Salary
table(Condt2)
prop.table(table(Condt2))

######## Applying RBFDOT Kernel ##########
model3 <- ksvm(Salary_train$Salary~., data = Salary_train, kernel = "rbfdot")
model3

Sal_pred3 <- predict(model3, Salary_Test)
table(Sal_pred3, Salary_Test$Salary)
Condt3 <- Sal_pred3 == Salary_Test$Salary
table(Condt3)
prop.table(table(Condt3))

######## Applying TANHDOT Kernel ##########
model4 <- ksvm(Salary_train$Salary~., data = Salary_train, kernel = "tanhdot")
model4

Sal_pred4 <- predict(model4, Salary_Test)
table(Sal_pred4, Salary_Test$Salary)
Condt4 <- Sal_pred4 == Salary_Test$Salary
table(Condt4)
prop.table(table(Condt4))

######## Applying LAPLACEDOT Kernel ##########
model5 <- ksvm(Salary_train$Salary~., data = Salary_train, kernel = "laplacedot")
model5

Sal_pred5 <- predict(model5, Salary_Test)
table(Sal_pred5, Salary_Test$Salary)
Condt5 <- Sal_pred5 == Salary_Test$Salary
table(Condt5)
prop.table(table(Condt5))







