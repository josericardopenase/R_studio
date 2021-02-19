  setwd("~/Escritorio/R/Datasets")
  
  educacion <- read.table("Datos_hsb.txt",sep = ",", header = TRUE, fileEncoding = "latin1")
  
  library(ggplot2)
  library(knitr)
  library(tidyr)
  
  library(extrafont)
  extrafont::font_import()
  
  #====================================
  # 
          #APARTADO A#
  #
  #====================================
  
  
  
  
  p <- aggregate(cbind(P_Lectura, P_Escritura, P_Matematicas, P_Sociales, P_Ciencias)~Estatus_Socieconomico, data = educacion, mean, na.rm=TRUE)
  kable(p)
  
  s <- aggregate(cbind(P_Lectura, P_Escritura, P_Matematicas, P_Sociales, P_Ciencias)~Tipo_Programa, data = educacion, mean, na.rm=TRUE)
  kable(s)
  
   p <- aggregate(cbind(P_Lectura, P_Escritura, P_Matematicas, P_Sociales, P_Ciencias)~Estatus_Socieconomico+Raza+Sexo, data = educacion, mean, na.rm=TRUE)
  kable(p)
 
  
  
  ########################
  #
  #       APARTADO B)
  #
  ########################
  
  
  
  
  educacion_escuela <- aggregate(cbind(P_Lectura, P_Escritura, P_Matematicas, P_Sociales, P_Ciencias)~Tipo_Escuela, data = educacion, mean, na.rm=TRUE)
  kable(p)
  centros <- unique(educacion$Tipo_Escuela); centros
  
  ggplot(data = educacion, mapping = aes(x = Tipo_Escuela, y = P_Ciencias)) + geom_boxplot() + aes(colour=Sexo)
  
  ggplot(data = educacion, mapping = aes(x = Tipo_Escuela, y = P_Matematicas)) + geom_boxplot() + aes(colour=Sexo)
  
  ggplot(data = educacion, mapping = aes(x = Tipo_Escuela, y = P_Lectura)) + geom_boxplot() + aes(colour=Sexo)
  
  ggplot(data = educacion, mapping = aes(x = Tipo_Escuela, y = P_Escritura)) + geom_boxplot() + aes(colour=Sexo)
  
  ggplot(data = educacion, mapping = aes(x = Tipo_Escuela, y = P_Sociales)) + geom_boxplot() + aes(colour=Sexo)
  
  
  #DIAGRAMAS DE BARRAS
  
 
  
  
  
  
  ########################
  #
  #       APARTADO C)
  #
  ########################
  
  
   #POR AHORA NO SE COMO HACERLO
   
    
  ########################
  #
  #       APARTADO D)
  #
  ########################
  
  
  
 
  #socioeconomico
  educacion2 <- educacion %>%
    gather(Campo,Value,  P_Ciencias, P_Escritura, P_Lectura, P_Matematicas, P_Sociales, -Estatus_Socieconomico)
  
  stat1 <- ggplot(educacion2, aes(x = Campo, y =Value , fill=Estatus_Socieconomico)) + geom_boxplot(position="dodge")
  stat1 + labs(title="Nota de las asignaturas en escuela privada y publica", x = "Asignaturas", y = "Nota") 
   p <- aggregate(cbind(P_Lectura, P_Escritura, P_Matematicas, P_Sociales, P_Ciencias)~Estatus_Socieconomico, data = educacion, mean, na.rm=TRUE)
   kable(p)
  
 