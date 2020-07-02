library(arules)
library(readxl)
library(arulesViz)
 
book_data <- read.csv(file.choose())
View(book_data)

arule <- apriori(as.matrix(book_data[1:11]), parameter = list(support = 0.06, confidence = 0.9))

arule1 <- apriori(as.matrix(book_data[1:11]), parameter = list(support = 0.08, confidence = 0.6))

arule2 <- apriori(as.matrix(book_data[1:11]), parameter = list(support = 0.07, confidence = 0.7))

inspect(sort(arule1, by="lift"))

plot(arule1[1:30], jitter= 0)
plot(arule1[1:30], method = "grouped")
plot(arule1[1:30], method = "graph")

######### Updated diffrent types of plots #########
library(colorspace)
plot(arule1, control=list(col=sequential_hcl(100)))

plot(arule1, shading="order", control=list(main = "Two-key plot", 
                                          col=rainbow(5)))
plot(arule1, method="matrix", measure=c("lift", "confidence"))

plot(arule1, method="paracoord")
