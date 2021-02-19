
#######################
#
#     SETUP
#
######################


data <- c(12.08, 4.84, 5.16, 12.88, 7.41, 4.19, 3.87, 4.84, 2.42, 5.80,
          7.06, 5.48, 7.73, 13.85, 2.34, 4.19, 4.04, 4.84, 0.32, 2.58,
          3.56 ,3.54, 0.97, 4.83, 1.62, 6.44, 14.66, 2.58, 3.87, 3.54)
length(data)

#######################
#
#     A)
#
######################

data_intervalos <- cut(data, breaks= seq(0,31, 5.85 ), include.lowest = TRUE)

frame <- data.frame(intervalos = data_intervalos, valor = data)

plot(frame$intervalos,frame$valor)
boxplot(data)
hist(data)

#######################
#
#     B)
#
######################

mean(data)
median(data)
sd(data)


#######################
#
#     C)
#
######################

length(data[data > 10])

#######################
#
#     D)
#
######################



mean(quantile(data, 0.25))

min(quantile(data, 0.25))
min(data)



#######################
#
#     E)
#
######################


quantile(data)
quantile(data, 0.95)
rango_interdecil <- quantile(data, 0.90) - quantile(data, 0.10); rango_interdecil


#######################
#
#     G)
#
######################

library(reldist)
gini(data)

#######################
#
#     G)
#
######################
library(e1071)
skewness(data)
kurtosis(data)



