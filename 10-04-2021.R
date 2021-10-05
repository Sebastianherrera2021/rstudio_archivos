#variables dummies
#VARIABLE CATEGORICA, TRANSFORMACION "
##DARLES UN VALOR 0 1 DEPENDIENDO DE CADA VARIABLE
#MALO MAYOR FRECUENCIA
###P REGULAR
#MATRIZ NO ES INVERTIBLE
#TOMAR SOLO TRES CATEGORIAS
#RELACION EXCELENTE -10 FELICIDAD COMPARADO CON REGULARES
#(INTERCEPTO-PBUENO*1)+/INTERCEPTO-PEXC*0) 65
#P-VALOR#################
#1,5,10 %, LAS VARIABLES EXPLICATIVAS SON BUENAS
#SOLAMENTE EL HECHO DE QUE LOS PADRES TENGAN RELACION EXCELENTE
#AFECTA A LA FELICIDAD SIN IMPORTAR LAS OTRAS.
##############PROMEDIO#######
#ESTRATO VARIABLE CATEGORICA
#r cuadrado ajustado "multiple" 0.47 no explica modelo
#p valor padres no afecta
#r cuadrado solo "simple"
#practica mirar el ajustado
#r cuadrado ajustado si pasa los 0.4 "en la practica"
#menor error estandar, cuadratico medio
#aic debe ser el menor
#ser mujer trae mnenos puntajes y estaria cerca de ser significativo "0.49999 hacia abajo"
#categorizar con una base de datos grande
#estrato categorica ordinal
#sexo categorica no ordinal
#puedo volver una variable continua 1 2 3 saltos
#volver estrato de continuo a categorico
#one hot encoding
#fast dummies paquete descargar
library(readxl)
ejemplo_dummy <- read_excel("C:/Users/Sebastián Rangel/Desktop/ejemplo dummy.xlsx")
View(ejemplo_dummy)
attach(ejemplo_dummy)
names(ejemplo_dummy)
summary(lm(felicidad~padres+as.character(estrato)))
table(estrato)
###   one hot encoding     
library(fastDummies)
datos2=data.frame(ejemplo_dummy , dummy_cols(padres))

###################################
#library(readxl)
ejemplo_dummy <- read_excel("C:/Users/Sebastián Rangel/Desktop/ejemplo dummy.xlsx")
View(ejemplo_dummy)
attach(ejemplo_dummy)
names(ejemplo_dummy)
summary(lm(felicidad~padres+as.character(estrato)))
table(estrato)
###   one hot encoding     
library(fastDummies)
datos2=data.frame(ejemplo_dummy , dummy_cols(padres))
attach(datos2)
names(datos2)
lm(felicidad~.data_Excelente, data=datos2)

