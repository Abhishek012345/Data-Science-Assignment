library(ggplot2)    
library(glmnet)     
bank <-read.csv(file.choose(), sep = ';')

set.seed(561)         


bank$job <- ifelse(bank$job == "unknown", 1, 0)
bank$edu <- ifelse(bank$education == "unknown", 1, 0)
bank$cont <- ifelse(bank$contact == "unknown", 1, 0)
bank$pout <- ifelse(bank$poutcome == "unknown", 1, 0)
bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$default<- ifelse(bank$default == "yes", 1, 0)
bank$housing <- ifelse(bank$housing== "yes", 1, 0)
bank$loan<- ifelse(bank$loan== "yes", 1, 0)
bank$month <- as.numeric(as.factor(bank$month))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))
bank$y <- ifelse(bank$y== "yes", 1, 0)



normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
bank <- as.data.frame(lapply(bank, normalize))


mydata.X <- model.matrix(y ~ -1+., data= bank)
mydata.X <- as.data.frame(mydata.X)
mydata.Y <- bank$y


cuts <- c(training = .8, test = .2)
g <- sample(cut(seq(nrow(mydata.X)), nrow(mydata.X)*cumsum(c(0,cuts)), labels = names(cuts)))
final.X <- split(mydata.X, g)
final.Y <- split(mydata.Y, g)


bank.ridge <- cv.glmnet(x= as.matrix(final.X$training), y = as.matrix(final.Y$training), nfolds=10, 
                        type.measure="class", family='binomial', alpha = 0, nlambda=100)
print(bank.ridge$lambda.min)

plot(bank.ridge)

ridge.coefs <- as.data.frame(as.vector(coef(bank.ridge, s = bank.ridge$lambda.min)), 
                             row.names = rownames(coef(bank.ridge)))
names(ridge.coefs) <- 'coefficient'


bank.lasso <- cv.glmnet(x = as.matrix(final.X$training), y = as.matrix(final.Y$training), nfolds=10, 
                        type.measure="class", parallel=TRUE, family='binomial', alpha = 1, nlambda=100)
print(bank.lasso$lambda.min)

plot(bank.lasso)

lasso.coefs <- as.data.frame(as.vector(coef(bank.lasso, s = bank.lasso$lambda.min)), 
                             row.names = rownames(coef(bank.lasso)))
print(lasso.coefs)

names(lasso.coefs) <- 'coefficient'


features <- rownames(lasso.coefs)[lasso.coefs != 0]
print(features)



lasso_bank <- bank[, intersect(colnames(bank), features)]

bank <- as.matrix(lasso_bank)
bank <- as.data.frame(bank)
bank$Y <- mydata.Y
bank_1 <- split(bank, g)


model_std <- glm(Y ~ ., family = binomial(link = "logit"),  data = bank_1$training)
summary(model_std)

names(model_std$coefficients)


predictions <- predict.glm(model_std, newdata=bank_1$test, type= "response")
predictions[predictions > 0.5] <- 1
predictions[predictions <= 0.5] <- 0
1 - length(predictions[predictions == bank_1$test$Y]) / length(predictions)

table(predictions, bank_1$test$Y)

library(gmodels)
CrossTable(predictions, bank_1$test$Y, prop.chisq = FALSE)
