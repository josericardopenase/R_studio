search()

library(help=datasets)

library(datasets)
library(rmarkdown)
library(knitr)

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

media_pesos<-c()

for(x in 0:length(unique(data$feed))){
    
  media_pesos[x] = mean(data$weight[data$feed == unique(data$feed)[x]])
    
}

tabla_medias = data.frame(feed = unique(data$feed), mean_weight = media_pesos )

kable(tabla_medias)

