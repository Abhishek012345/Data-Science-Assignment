View(Company_Data)
str(Company_Data)
library(C50)
library(caret)
high = ifelse(Company_Data$Sales<10,"no","yes")

library(dummies)
Company_Data.new = dummy.data.frame(Company_Data)
dummy[ShelveLoc=="Bad"]==0
dummy[ShelveLoc=="Medium"]==1
dummy[ShelveLoc=="Good"]==2
dummy[Urban=="Yes"]==1
dummy[Urban=="No"]==0
dummy[US=="No"]==0
dummy[US=="Yes"]==1
View(Company_Data.new)

final = data.frame(Company_Data.new,high)
View(final)
final1 = final[-1]
View(final1)

round(prop.table(table(final1$high))*100, digits = 1)

normalize <- function(x)
{
  return((x-min(x)) / max(x) - min(x))
}
final1_norm <- as.data.frame(lapply(final1[1:14], normalize))
trainingloc <- createDataPartition(final1$high, p=.80, list = F)

training <- final1[trainingloc,]
testing <- final1[-trainingloc,]

model1 <- C5.0(training$high~., data = training, trails=1000)
summary(model1)
 
pred <- predict.C5.0(model1,testing)
a <- table(testing$high,pred)
View(a)
confusionMatrix(a)
plot(model1)
