setwd("~/Escritorio/R/Practica 1")

empleados <- read.table("empleados.txt",sep = ",", header = TRUE)
salarios <- read.table("salarios.txt",sep = "\t",header = TRUE)

names(empleados)
names(salarios)

#a)
head(empleados)
tail(empleados)

head(salarios)
tail(salarios)

salario_medio_por_empleado = c()
salario_med = c()
salario_dp = c()

for(x in 0:length(unique(salarios$Num_Empleado))){
   salario_medio_por_empleado[x] = mean(salarios$Salario[salarios$Num_Empleado == unique(salarios$Num_Empleado)[x]])
   salario_med[x] = median(salarios$Salario[salarios$Num_Empleado == unique(salarios$Num_Empleado)[x]])
   salario_dp[x] = sd(salarios$Salario[salarios$Num_Empleado == unique(salarios$Num_Empleado)[x]])
   
}




sm <- data.frame(empleado = toString(unique(salarios$Num_Empleado)), sueldo_medio = salario_medio_por_empleado, mediana = salario_med, desviacion_tipica = salario_dp)
sm
##EMPLEADO CON MAYOR SUELDO
sm$empleado[which.max(sm$sueldo_medio)]
sm$empleado[which.min(sm$sueldo_medio)]


#usando boxplot
boxplot(salarios$Salario, salarios$Num_Empleado)

#usando merge
newData <- merge(salarios, empleados, all=TRUE)

newData




