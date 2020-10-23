library(carData)
library(knit)

p <- Leinhardt

p <- na.omit(p)

summary(p)


row.names(p)

p <- data.frame(p, pais = row.names(p))

names(p) <- c("renta", "mortalidad_infantil", "region", "productor_petroleo", "pais")

head(p)

p$pais[which.min(p$mortalidad_infantil)]

p$pais[which.max(p$mortalidad_infantil)]



kable(aggregate(cbind(r)))