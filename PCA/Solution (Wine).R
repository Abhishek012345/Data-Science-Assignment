library(cluster)
install.packages("factoextra")
library(factoextra)

mydata <- read.csv(file.choose())
View(mydata)
View(mydata[,-1])
data <- mydata[,-1]
attach(data)
cor(data)

pca<- princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
str(pca)
summary(pca)
loadings(pca)

plot(pca)
pca$scores[,1:3]
mydata <- cbind(mydata, pca$scores[,1:3])
View(mydata)

######### Performing Hierarchial Clustering ##############
clus_data <- mydata[,8:10]
norm_clus <- scale(clus_data)
dist1 <- dist(norm_clus, method = "euclidean")
fit1 <- hclust(dist1, method = "complete")
plot(fit1)

rect.hclust(fit1, k=7, border = "blue")
groups <- cutree(fit1, 7)
member1 <- as.matrix(groups)
View(member1)

final1<-cbind(member1,mydata) 
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(member1),FUN=mean))

########## Performing K - Means Clustering ##############
library(plyr)
mydata <- read.csv(file.choose())
View(mydata)

normalize_data <- scale(final1[,15:17])
wss = (nrow(normalize_data)-1)*sum(apply(normalize_data, 2, var))    
for (i in 1:7) wss[i] = sum(kmeans(normalize_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  
title(sub = "K-Means Clustering Scree-Plot")

fit <- eclust(normalize_data, "kmeans", k = 7, nstart = 25, graph = FALSE) 
fviz_cluster(fit, geom = "point", frame.type = "norm")

final2<- data.frame(fit$cluster,mydata) 
View(final2)
aggregate(final1[,2:17], by=list(fit$cluster), FUN=mean)
table(fit$cluster)


