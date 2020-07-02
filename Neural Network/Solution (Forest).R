library(neuralnet)  
library(nnet)

forestfires$size_category <- as.numeric(forestfires$size_category,
                             c("small"="0", "large"="1"))

forestfires = forestfires[-c(1:2)]
View(forestfires)
str(forestfires)
forestfires <- as.data.frame(forestfires)
attach(forestfires)

plot(size,temp)
cor(forestfires)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

frst_norm<-as.data.frame(lapply(forestfires,FUN=normalize))

summary(forestfires$size) 
set.seed(123)

ind <- sample(2, nrow(frst_norm), replace = TRUE, prob = c(0.7,0.3))
frst_train <- frst_norm[ind==1,]
frst_test  <- frst_norm[ind==2,]

frst_model <- neuralnet(size_category ~ temp + RH + wind + rain,data = frst_train)
str(frst_model)

plot(frst_model)

set.seed(12323)
model_results <- neuralnet::compute(frst_model,frst_test)
predicted_profit <- model_results$net.result

cor(predicted_profit,frst_test$size_category)

str_max <- max(forestfires$size_category)
str_min <- min(forestfires$size_category)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

set.seed(12345)

frst_model2 <- neuralnet(size_category ~ temp + RH + wind + rain,data = frst_train,
                             hidden = 2)
plot(frst_model2)

summary(frst_model2)

model_result2 <- neuralnet::compute(frst_model2,frst_test)
predicted_profit2 <- model_result2$net.result
cor(predicted_profit2,frst_test$size_category)

######### Using LOGISTIC Activation Functions ##############
frst_model3 <- neuralnet(size_category ~ temp + RH + wind + rain,data = frst_train,
                         hidden = 2, act.fct = "logistic")
plot(frst_model3)

summary(frst_model3)

model_result3 <- neuralnet::compute(frst_model3,frst_test)
predicted_profit3 <- model_result3$net.result
cor(predicted_profit3,frst_test$size_category)

######### Using TANH Activation Functions ##############
frst_model4 <- neuralnet(size_category ~ temp + RH + wind + rain,data = frst_train,
                         hidden = 2, act.fct = "tanh")
plot(frst_model4)

summary(frst_model4)

model_result4 <- neuralnet::compute(frst_model4,frst_test)
predicted_profit4 <- model_result4$net.result
cor(predicted_profit4,frst_test$size_category)


