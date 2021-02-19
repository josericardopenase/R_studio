
####################
#
#  SETUP INICIAL #
#
####################
library(ggplot2)
barrio_b1 <- c(941 ,1086 ,1024, 1278, 1183, 1419, 1384, 1318, 1140, 1170,
               1137, 1327, 1225, 1121, 1114, 1088, 1086, 1500 ,910 ,1750)  


barrio_b2 <- c(959, 1080, 2101, 1265, 1247, 1475, 1023, 1651, 2123, 1722,
               2888, 2010, 793 ,1521, 1927 ,2064, 1680 ,1882, 1790 ,960) 

barrio_b3 <- c(1523 ,779, 2031 ,694 ,2900 ,774 ,1797, 878, 1850 ,960
,1010, 1526 ,981 ,1685, 889 ,1063 ,1113 ,1754, 884 ,1009
,1524 ,930 ,1319 ,1229, 1106, 1323, 1382, 1171, 1567, 1600)


letras <- c()

for(x in 0:(length(barrio_b1))){
 letras[x] = "B1" 
}

for(x in 21 : 40){
   letras[x] = "B2" 
}

for(x in 40 : 70){
 letras[x] = "B3" 
}
 
data <- data.frame(barrio = letras, ingresos=c(barrio_b1 , barrio_b2, barrio_b3))

data

####################
#
#  APARTADO A) #
#
####################




s1 <- plot(as.factor(data$barrio), data$ingresos); s1 

min(data$ingresos)
max(data$ingresos)

intervalosRenta <- cut(data$ingresos, breaks=seq(694, 2900, 220.6), include.lowest=TRUE, right=TRUE)

intervalosRenta
data <- data.frame(interval = intervalosRenta, renta = data$ingresos, barrio = data$barrio)

data
#tabla de frecuencias absolutas
rep <- hist(data$renta, breaks=seq(694, 2900, 220.6))
tabla_frecuencias <- data.frame(table(data$interval), rep$mids) 
names(tabla_frecuencias) <- c("Intervalos", "Freq. Absol", "Representantes de clase")
tabla_frecuencias <- data.frame(tabla_frecuencias, tabla_frecuencias$`Freq. Absol`/length(data$renta)) 
names(tabla_frecuencias) <- c("Intervalos", "Freq. Absol", "Representantes de clase", "Freq relativa")
tabla_frecuencias



####################
#
#  APARTADO B) #
#
####################


#MEDIAS Y SD GENERALES

mean(data$renta)
sd(data$renta)

#MEDIAS Y SD POR BARRI  O

tabla_medias <- aggregate(renta~barrio, data=data, mean )
names(tabla_medias) <- c("Barrio", "Media renta")
tabla_sd <- aggregate(renta~barrio, data=data, sd )
names(tabla_sd) <- c("Barrio", "Desviacion estandar")

tabla_final <- merge(tabla_medias, tabla_sd)
tabla_final





####################
#
#  APARTADO C) #
#
####################



renta <- sort(data$renta)

median(renta)
quantile(renta)

recorrido_intercuartil <- quantile(renta, 0.75) - quantile(renta, 0.25)
recorrido_intercuartil



####################
#
#  APARTADO D) #
#
####################



plot(as.factor(data$barrio),data$renta )

hist(data$renta, main="Histograma de la renta", xlab="Renta", ylab="Frecuencia")


hist(data$renta[data$barrio=="B1"], main="Histograma renta en B1", xlab="Renta", ylab="Frecuencia") 

hist(data$renta[data$barrio=="B2"], main="Histograma renta en B1", xlab="Renta", ylab="Frecuencia") 

hist(data$renta[data$barrio=="B3"], main="Histograma renta en B1", xlab="Renta", ylab="Frecuencia") 



####################
#
#  APARTADO E y F) #
#
####################


graph <- plot(as.factor(data$barrio),data$renta )

graph$out
graph






####################
#
#  APARTADO H y I) #
#
####################

ggplot(data = data, aes(barrio, renta)) + geom_boxplot() 
ggplot(data = data, aes(intervalosRenta, renta)) + geom_boxplot() 
