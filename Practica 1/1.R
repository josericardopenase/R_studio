search()

library(help=datasets)

library(datasets)
library(rmarkdown)

data <- chickwts 

names(data)
head(data)
tail(data)
table(data)

summary(data)


typeof(data)
typeof(data$weight)
typeof(data$feed)

ls()

media <- sum(data$weight)/length(data$weight);media
calculo <- (max(data$weight) + min(data$weight))/2;calculo

plot( data$feed, data$weight, 
      main="Peso en funcion del metodo de alimento",
      ylab="kilograms",
      xlab="feeding type",
      col="green")


