library(neuralnet)  
library(nnet) 

Startups <- read.csv(file.choose())
View(Startups)

Startups$State <- as.numeric(Startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2"))
str(Startups)
Startups <- as.data.frame(Startups)
attach(Startups)

plot(R.D.Spend, Profit)
pairs(Startups)

cor(Startups)

summary(Startups) 

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))
summary(Startups_norm$Profit) 

set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]


startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(startups_model)

plot(startups_model, rep = "best")


summary(startups_model)
par(mar = numeric(4), family = 'serif')
plotnet(startups_model, alpha = 0.6)


set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result

cor(predicted_profit,startups_test$Profit)

str_max <- max(Startups$Profit)
str_min <- min(Startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

set.seed(12345)
Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2)
plot(Startups_model2 ,rep = "best")


summary(Startups_model2)

model_results2<-compute(Startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)

plot(predicted_Profit2,startups_test$Profit)

######### Using LOGISTIC Activation Functions ##############
Startups_model3 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2, act.fct = "logistic")
plot(Startups_model3 ,rep = "best")


summary(Startups_model3)

model_results3<-compute(Startups_model3,startups_test[1:4])
predicted_Profit3<-model_results3$net.result
cor(predicted_Profit3,startups_test$Profit)

plot(predicted_Profit3,startups_test$Profit)

######### Using TANH Activation Functions ##############
Startups_model4 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2, act.fct = "tanh")
plot(Startups_model4 ,rep = "best")


summary(Startups_model4)

model_results4<-compute(Startups_model4,startups_test[1:4])
predicted_Profit4<-model_results4$net.result
cor(predicted_Profit4,startups_test$Profit)

plot(predicted_Profit4,startups_test$Profit)







