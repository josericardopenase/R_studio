setwd("~/Escritorio/R/Practica 1")
melanoma <- read.table("Datos_Melanoma.txt",sep = "\t", header = TRUE)

#INSPECCIONAMOS LA TABLA DE DATOS

head(melanoma)
tail(melanoma)
summary(melanoma)

#REALIZAMOS UNA VISTA GENERAL DE EL GROSOR EN FUNCION DEL TIEMPO
#Observamos que la cantidad de esperanza de vida
#tiene alta relacion con el grosor del melanoma
#al mientras mas ser el tamaño de este menor ser la esperanza

plot(melanoma$grosor, melanoma$tiempo, 
     xlab="Grosor melanoma", 
     ylab="Tiempo de supervivencia",
     main="Tiempo de supervivencia en funcion del grosor del melanoma")

#Observamos los diferentes tamaños que tenemos
sort(unique(melanoma$grosor))

melanoma_agrupado <- cut(melanoma$grosor, breaks=seq(0, 20, 5), include.lowest=TRUE, right=TRUE)

tabla_agrupado <- data.frame(size = melanoma_agrupado, tiempo = melanoma$tiempo, ulceracion = melanoma$ulceracion, sexo=melanoma$sexo, edad=melanoma_agrupado_edad)


plot(tabla_agrupado$size, tabla_agrupado$tiempo)

periodos <- unique(tabla_agrupado$size)

#CALCULAMOS MEDIAS DE DURACION EN CADA PERIODO

#las medianas indican que la gran diferencia
#se nota a partir de los 10mm de grosor
#Aunque los datos la mayoría se tomaron de melanomas menores a 5mm
mean(tabla_agrupado$tiempo[tabla_agrupado$size == periodos[1]]) 
mean(tabla_agrupado$tiempo[tabla_agrupado$size == periodos[2]])
mean(tabla_agrupado$tiempo[tabla_agrupado$size == periodos[3]])
mean(tabla_agrupado$tiempo[tabla_agrupado$size == periodos[4]])

#CALCULAMOS LAS MEDIANAS EN CADA PERIODO
median(tabla_agrupado$tiempo[tabla_agrupado$size == periodos[1]])
median(tabla_agrupado$tiempo[tabla_agrupado$size == periodos[2]])
median(tabla_agrupado$tiempo[tabla_agrupado$size == periodos[3]])
median(tabla_agrupado$tiempo[tabla_agrupado$size == periodos[4]])

#CONTAMOS LA CANTIDAD DE DATOS RECOGIDOS DE CADA
length(melanoma$grosor[melanoma$grosor < 5]) #demasiados datos de este
length(melanoma$grosor[melanoma$grosor > 5]) #muy poocos de este





#==============================

  #EFECTO SEGÚN LA EDAD#

#==============================
melanoma_agrupado_edad <- cut(melanoma$edad, breaks=seq(0, 100, 15), include.lowest=TRUE, right=TRUE)


sort(unique(melanoma$edad))

plot(  melanoma$edad, melanoma$tiempo)



melanoma_agrupado_frame <- na.omit(data.frame(edad = melanoma_agrupado_edad, tiempo = melanoma$tiempo))


plot(melanoma_agrupado_frame$edad, melanoma_agrupado_frame$tiempo)

m_tp_vida_edad = c()

for(x in 1:length(unique(melanoma_agrupado_edad))){
  m_tp_vida_edad[x] = mean(melanoma_agrupado_frame$tiempo[melanoma_agrupado_frame$edad == unique(melanoma_agrupado_edad)[x]])
}

m_tp_vida_edad <- m_tp_vida_edad

mean_edad_mortalidad <- na.omit(data.frame(edad = unique(melanoma_agrupado_edad), media_vida = m_tp_vida_edad))

mean_edad_mortalidad

#==============================

#ULCERACION#

#==============================

#a)

#CON ULCERACION

plot(tabla_agrupado$size[tabla_agrupado$ulceracion == 1], tabla_agrupado$tiempo[tabla_agrupado$ulceracion == 1])
plot(tabla_agrupado$edad[tabla_agrupado$ulceracion == 1], tabla_agrupado$tiempo[tabla_agrupado$ulceracion == 1])
mean(tabla_agrupado$tiempo[tabla_agrupado$ulceracion == 1])
median(tabla_agrupado$tiempo[tabla_agrupado$ulceracion == 1])
length(melanoma$tiempo[melanoma$ulceracion == 1])

#SIN ULCERACION
plot(tabla_agrupado$size[tabla_agrupado$ulceracion == 0], tabla_agrupado$tiempo[tabla_agrupado$ulceracion == 0])
plot(tabla_agrupado$edad[tabla_agrupado$ulceracion == 0], tabla_agrupado$tiempo[tabla_agrupado$ulceracion == 0])
mean(tabla_agrupado$tiempo[tabla_agrupado$ulceracion == 0])
median(tabla_agrupado$tiempo[tabla_agrupado$ulceracion == 0])
length(melanoma$tiempo[melanoma$ulceracion == 0])

#CLARAMENTE LA ULCERACION AFECTA A LA ESPERANZA DE VIDA la esperanza de vida promedio se extiende bastante mas

#HOMBRE
plot(tabla_agrupado$size[tabla_agrupado$sexo == 0], tabla_agrupado$tiempo[tabla_agrupado$sexo == 0])
plot(tabla_agrupado$edad[tabla_agrupado$sexo == 0], tabla_agrupado$tiempo[tabla_agrupado$sexo == 0])
mean(tabla_agrupado$tiempo[tabla_agrupado$sexo == 0])
median(tabla_agrupado$tiempo[tabla_agrupado$sexo == 0])
length(melanoma$tiempo[melanoma$sexo == 0])

#MUJER

plot(tabla_agrupado$size[tabla_agrupado$sexo == 1], tabla_agrupado$tiempo[tabla_agrupado$sexo == 1])
plot(tabla_agrupado$edad[tabla_agrupado$sexo == 1], tabla_agrupado$tiempo[tabla_agrupado$sexo == 1])
mean(tabla_agrupado$tiempo[tabla_agrupado$sexo == 1])
median(tabla_agrupado$tiempo[tabla_agrupado$sexo == 1])
length(melanoma$tiempo[melanoma$sexo == 1])



