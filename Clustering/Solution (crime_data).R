crime0 <- na.omit(USArrests)
crime <- data.matrix(crime0)
str(crime)

cl <- kmeans(crime, 5)
class(cl)
str(cl)

kmeans.wss.k <- function(crime, k){
  km = kmeans(crime, k)
  return(km$tot.withinss)
}
kmeans.wss.k(crime, 5)
kmeans.wss.k(crime, 10)

kmeans.dis <- function(crime, maxk){
  dis=(nrow(crime)-1)*sum(apply(crime,2,var))
  dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime=crime)
  return(dis)
}
maxk = 10
dis = kmeans.dis(crime, maxk)
plot(1:maxk, dis, type='b', xlab="Number of Clusters",
      ylab="Distortion", col="blue")

########### Updated Elbow Curve for K-Means Clustering ########
library(animation)
cl <- kmeans.ani(crime, 4)

######### Updated Scree Plot for k-Means Clustering #######
pc.cr <- princomp(USArrests, cor = TRUE)
screeplot(pc.cr)
fit <- princomp(covmat = Harman74.cor)
screeplot(fit, npcs = 24, type = "lines")

##### write a loop for different k values and plot the accuracies ######
wss <- 0
for (i in 1:15) {
   cl <- kmeans(crime, centers = i, nstart = 20)  
   wss[i] <- cl$tot.withinss
}

plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")






