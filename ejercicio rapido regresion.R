##otra forma de guardar
delitosb<-'https://www.datos.gov.co/resource/75fz-q98y.csv'
datos<-read.csv(delitosb)
View(datos)
attach(datos)
## usar variables años y edad, nota= x=puede ser cualquiera, y=continua
library(ggplot2)
names(datos)
regresion<-lm( as.numeric(edad) ~ armas_medios)
summary(regresion)

