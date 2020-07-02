library(kernlab)
library(caret)
library(ggplot2)
library(dplyr)

attach(forestfires)
str(forestfires)
hist(forestfires$area)
frst_frs <- mutate(forestfires, y = log(area+1))
hist(frst_frs$y)

summary(frst_frs)

normalize <- function(x)
{
  return((x-min(x)) / max(x) - min(x))
}
forestfires$RH = normalize(forestfires$RH)
forestfires$temp = normalize(forestfires$temp)
forestfires$rain = normalize(forestfires$rain)
forestfires$wind = normalize(forestfires$wind)

set.seed(244)
trn_tst <- sample(2, nrow(forestfires), replace = TRUE, prob = c(0.7,0.3))
forest_train <- forestfires[trn_tst==1,]
forest_test <- forestfires[trn_tst==2,]


model1 <- ksvm(size_category~temp + rain + wind + RH, data= forest_train, kernel= "vanilladot")
model1

pred_ar <- predict(model1, forest_test)
table(pred_ar, forest_test$size_category)

green <- pred_ar == forest_test$size_category
table(green)
prop.table(table(green))

######## Applying POLYDOT Kernel ##########
model2 <- ksvm(size_category~temp + rain + wind + RH, data= forest_train, kernel= "polydot")
model2

pred_ar2 <- predict(model2, forest_test)
table(pred_ar2, forest_test$size_category)

green2 <- pred_ar2 == forest_test$size_category
table(green2)
prop.table(table(green2))

######## Applying RBFDOT Kernel ##########
model3 <- ksvm(size_category~temp + rain + wind + RH, data= forest_train, kernel= "rbfdot")
model3

pred_ar3 <- predict(model3, forest_test)
table(pred_ar3, forest_test$size_category)

green3 <- pred_ar3 == forest_test$size_category
table(green3)
prop.table(table(green3))

######## Applying TANHDOT Kernel ##########
model4 <- ksvm(size_category~temp + rain + wind + RH, data= forest_train, kernel= "tanhdot")
model4

pred_ar4 <- predict(model4, forest_test)
table(pred_ar4, forest_test$size_category)

green4 <- pred_ar4 == forest_test$size_category
table(green4)
prop.table(table(green4))

######## Applying LAPLACEDOT Kernel ##########
model5 <- ksvm(size_category~temp + rain + wind + RH, data= forest_train, kernel= "laplacedot")
model5

pred_ar5 <- predict(model5, forest_test)
table(pred_ar5, forest_test$size_category)

green5 <- pred_ar5 == forest_test$size_category
table(green5)
prop.table(table(green))


