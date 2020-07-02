library("arules")
library("readxl")

movie_data <- read.csv(file.choose())
View(movie_data)

library("arulesViz")
arule <- apriori(as.matrix(movie_data[6:15]), parameter = list(support = 0.2, confidence = 0.7))

arule1 <- apriori(as.matrix(movie_data[6:15]), parameter = list(support = 0.06, confidence = 0.8))

arule2 <- apriori(as.matrix(movie_data[6:15]), parameter = list(support = 0.03, confidence = 0.6))

inspect(sort(arule, by="lift"))

plot(arule, jitter=0)
plot(arule, method = "grouped")
plot(arule, method = "graph")

######### Updated diffrent types of plots #########
library(colorspace)
plot(arule, control=list(col=sequential_hcl(100)), jitter = 0)

plot(arule, shading="order", control=list(main = "Two-key plot", 
                                          col=rainbow(5)), jitter = 0)

plot(arule, method="matrix", measure=c("lift", "confidence"))

plot(arules, method="paracoord")





