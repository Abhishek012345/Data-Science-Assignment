Cust_order <- read.csv(file.choose())
attach(Cust_order)

table1 <- table(Phillippines, Indonesia, Malta, India)
View(table1)
summary(Cust_order)
prop.test(x=c(20,33),n=c(247,206),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
prop.test(x=c(20,33),n=c(247,206),conf.level = 0.95,correct = FALSE,alternative = "less")
