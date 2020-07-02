library(readxl)
Air_data <- read_excel(file.choose(),2)

data <- Air_data[,(2:12)]
data.first50 <- data[1:50,]

View(data)

normalize <- scale(data.first50)
normalize
View(data.first50)
d <- dist(normalize, method = "euclidean")
?dist
?hclust
fit<- hclust(d, method = "complete")
fit
plot(fit)
plot(fit, hang = -1)
######### Updated MAXIMUM Distance Measures #########
d1 <- dist(normalize, method = "maximum")
fit1 <- hclust(d1, method = "single")
fit1
plot(fit1)
plot(fit1, hang = -1)
group <- cutree(fit1, k=3)
rect.hclust(fit1, k=3, border = "red")

######### Updated MANHATTAN Distance Measures #########

d2 <- dist(normalize, method = "manhattan")
fit2 <- hclust(d2, method = "average")
fit2
plot(fit2)
plot(fit2, hang = -1)
group <- cutree(fit2, k=3)
rect.hclust(fit2, k=3, border = "red")

######### Updated CANBERRA Distance Measures #########
d3 <- dist(normalize, method = "canberra")
fit3 <- hclust(d3, method = "centroid")
fit3
plot(fit3)
plot(fit3, hang = -1)
group <- cutree(fit3, k=3)
rect.hclust(fit3, k=3, border = "red")
  
member <- as.matrix(group)
member
final <- data.frame(data.first50, member)
View(final)  
  