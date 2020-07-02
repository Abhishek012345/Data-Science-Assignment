normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
View(concrete)

concrete_norm <- as.data.frame(lapply(concrete, normalize))
View(concrete_norm)

concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

install.packages("neuralnet")
library(neuralnet)

concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)

plot(concrete_model)

concrete_test[1:8]
results_model <- compute(concrete_model, concrete_test[1:8])

str(results_model)
predicted_strength <- results_model$net.result
predicted_strength

cor(predicted_strength, concrete_test$strength)
######### Using LOGISTIC Activation Functions ##############

concrete_model2 <- neuralnet(strength ~ cement + slag +
                  ash + water + superplastic + 
                  coarseagg + fineagg + age, data = concrete_train, 
                  hidden = 10, act.fct = "logistic")

plot(concrete_model2)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

######### Using TANH Activation Functions ##############
concrete_model3 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                          coarseagg + fineagg + age, data = concrete_train, 
                          hidden = 10, act.fct = "tanh")

plot(concrete_model3)

model_results3 <- compute(concrete_model3, concrete_test[1:8])
predicted_strength3 <- model_results3$net.result
cor(predicted_strength3, concrete_test$strength)


