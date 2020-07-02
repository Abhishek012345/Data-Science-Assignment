library("arules")
library("readxl")

Book_data <- read.csv(file.choose())
View(Book_data)

library("arulesViz")
arules <- apriori(Book_data, parameter = list(support = 0.002, confidence=0.6))

arules1 <- apriori(Book_data, parameter = list(support = 0.06, confidence=0.8))

arules2 <- apriori(Book_data, parameter = list(support = 0.03, confidence=0.6))

inspect(sort(arules,by="count"))

plot(arules[1:30],jitter = 0)
plot(arules[1:20], method = "grouped")
plot(arules[1:12], method = "graph")

######### Updated diffrent types of plots #########
library(colorspace)
plot(arules, control=list(col=sequential_hcl(100)), jitter = 0)

plot(arules, shading="order", control=list(main = "Two-key plot", 
                                  col=rainbow(5)), jitter = 0)

plot(arules, method="matrix", measure=c("lift", "confidence"))

plot(arules, method="paracoord")



