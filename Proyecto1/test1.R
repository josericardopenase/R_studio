library(datasets)
library(modeest)

head(iris)
tail(iris)

?plot

plot(iris)

plot(iris$Petal.Length, iris$Petal.Width, 
     col= "#cc0000", 
     pch=19, 
     xlab = "petal length",
     ylab = "petal width",
     main= "cosa rara")


barplot(iris$Petal.Length)

tespecies<-table(iris$Species)

hist(iris$Petal.Length, breaks = c(0,2,4,6,8), density=10.5, col= "green", border= "brown")




table(iris$Sepal.Length, iris$Sepal.Width)

aggregate(iris$Sepal.Length~iris$Sepal.Width, iris, mean)

iris$Species

mean(iris$Sepal.Length[iris$Species=="setosa"])

mean(iris$Sepal.Length[iris$Species=="virginica"])

mean(iris$Sepal.Length[iris$Species=="versicolor"])


plot(iris$Species, iris$Sepal.Length)

sort(iris$Sepal.Length)

intervalosLongitud <- cut(iris$Sepal.Length, breaks=seq(4,8,1), include.lowest = TRUE, right= TRUE)

plot(intervalosLongitud)

mlv(iris$Species)

length(iris$Species[iris$Species=="setosa"])

