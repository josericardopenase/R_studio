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
divisiones_renta <-  cut(p$renta, breaks=seq(0, 3346, 500), include.lowest=TRUE, right=TRUE)

boxplot(mortalidad_infantil~renta, data = p, main="mortalidad infantil por rpc", xlab="renta per capita", ylab ="mortalidad infantil")

p$pais[p$renta]
p$renta[p$pais == "Saudi.Arabia"]

##F)

datos2 <- data.frame(renta = p$renta[p$productor_petroleo == "no"], mortalidad_infantil = p$mortalidad_infantil[p$productor_petroleo == "no"])

plot(datos2$renta, datos2$mortalidad_infantil)

mean(datos2$mortalidad_infantil)
mean(p$mortalidad_infantil[p$productor_petroleo=="yes"])

mean(datos2$renta)
mean(p$renta[p$productor_petroleo=="yes"])

datos_africa <- data.frame(pais = p$pais[p$region=="Africa"],renta = p$renta[p$region=="Africa"], mortalidad_infantil = p$mortalidad_infantil[p$region=="Africa"])


boxplot(mortalidad_infantil~renta, data = datos_africa, main="mortalidad infantil por rpc áfrica", xlab="renta per capita", ylab ="mortalidad infantil")

plot( datos_africa$renta, datos_africa$mortalidad_infantil,main="mortalidad infantil por rpc áfrica", xlab="renta per capita", ylab ="mortalidad infantil")

datos_africa$pais[datos_africa$renta > 800]

mortalidad_infantil_sum = c()

for(x in 0:length(unique(p$region))){
        mortalidad_infantil_sum[x] = sum(p$mortalidad_infantil[p$region==unique(p$region)[x]])
}

pie(mortalidad_infantil_sum, labels = unique(p$region))

