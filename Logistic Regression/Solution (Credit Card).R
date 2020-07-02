crdt <- read.csv(file.choose())
View(crdt)
data=crdt[,-1]
View(data)
crdt <- na.omit(crdt)
View(crdt)
model <- glm(card~.,data=crdt[,-1],family = "binomial")

prob <- predict(model,type=c("response"),crdt)
prob

confusion<-table(prob>0.5,crdt$card)
confusion

confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
sum(confusion[cbind(2:1, 1:2)])/sum(confusion)
?diag

pred_values <- NULL
yes_no <- NULL
for (i in 1:1319){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

crdt[,"prob"] <- prob
View(prob)
crdt[,"pred_values"] <- pred_values
View(pred_values)
crdt[,"yes_no"] <- yes_no
View(crdt)

View(crdt[,c(2,8,9,10)])

acc <- table(crdt$card,pred_values)
acc
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy

library(ROCR)
rocrpred<-prediction(prob,crdt$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))


