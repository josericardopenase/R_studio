library(carData)
library(knitr)

data <- Leinhardt

p <- na.omit(data)

summary(data)
head(data)
tail(data)

row.names(p)

p <- data.frame(p, pais = row.names(p))

names(p) <- c("renta", "mortalidad_infantil", "region", "productor_petroleo", "pais")

max(p$renta)

pais_mayor_renta <- p$pais[which.max(p$renta)];pais_mayor_renta
pais_menor_renta <- p$pais[which.min(p$renta)];pais_menor_renta

pais_mayor_mi <- p$pais[which.max(p$mortalidad_infantil)];pais_mayor_mi
pais_menor_mi <- p$pais[which.min(p$mortalidad_infantil)];pais_menor_mi

plot(p$renta, p$mortalidad_infantil, 
     xlab="renta", 
     ylab="mortalidad infantil",
     main="mortalidad en funcion de la renta")

plot(p$region, p$renta, 
     xlab="renta", 
     ylab="mortalidad infantil",
     main="renta en funcion del pais")

plot(p$region, p$mortalidad_infantil, 
     xlab="renta", 
     ylab="mortalidad infantil",
     main="renta en funcion del pais")

#MEDIA Y MEDIANA GLOBALES DE RENTA PER CAPITA

mean(p$renta)
median(p$renta)
sd(p$renta)

#MEDIAS Y MEDIANAS EN CADA REGION

mean(p$renta[p$region=="Asia"])
mean(p$renta[p$region=="Africa"])
mean(p$renta[p$region=="Europe"])
mean(p$renta[p$region=="Americas"])

median(p$renta[p$region=="Asia"])
median(p$renta[p$region=="Africa"])
median(p$renta[p$region=="Europe"])
median(p$renta[p$region=="Americas"])

#calculamos la desviacion estandar en cada region
sd(p$renta[p$region=="Asia"])
sd(p$renta[p$region=="Africa"])
sd(p$renta[p$region=="Europe"])
sd(p$renta[p$region=="Americas"])

#MEDIA Y MEDIANA GLOBALES DE MORTALIDAD INFANTIL

mean(p$mortalidad_infantil)
median(p$mortalidad_infantil)
sd(p$mortalidad_infantil)

#MEDIAS Y MEDIANAS EN CADA REGION

mean(p$mortalidad_infantil[p$region=="Asia"])
mean(p$mortalidad_infantil[p$region=="Africa"])
mean(p$mortalidad_infantil[p$region=="Europe"])
mean(p$mortalidad_infantil[p$region=="Americas"])

median(p$mortalidad_infantil[p$region=="Asia"])
median(p$mortalidad_infantil[p$region=="Africa"])
median(p$mortalidad_infantil[p$region=="Europe"])
median(p$mortalidad_infantil[p$region=="Americas"])

#calculamos la desviacion estandar en cada region
sd(p$mortalidad_infantil[p$region=="Asia"])
sd(p$mortalidad_infantil[p$region=="Africa"])
sd(p$mortalidad_infantil[p$region=="Europe"])
sd(p$mortalidad_infantil[p$region=="Americas"])

boxplot(p$mortalidad_infantil)

#SE PUEDE OBSERVAR CLARAMENTE QUE LA RENTA PER CAPITA Y LA MORTALIDAD
#INFANTIL ESTAN FUERTEMENTE RELACIONADAS.