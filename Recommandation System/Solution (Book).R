install.packages("recommenderlab", dependencies=TRUE)
library(Matrix)
library(recommenderlab)
library(caTools)

str(book)
View(book)
hist(book$Book.Rating)
book_data_matrix <- as(book, 'realRatingMatrix')

book_recomm_model <- Recommender(book_data_matrix, method = "POPULAR")
recomm_items1 <- predict(book_recomm_model, book_data_matrix[1])
as(recomm_items1, "list")

book_recomm_model2 <- Recommender(book_data_matrix, method = "UBCF")
recomm_items2 <- predict(book_recomm_model2, book_data_matrix[1], n=4)

as(recomm_items2, "list")






