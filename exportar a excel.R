rm(list=ls()) ##borrar environment
library(readxl) ##leer
library(tidyr) ## ordenar datos desorganizados
cerve<-(read_excel("C:Users/ADMIN/Downloads/hsranqui_cerveza.xlsx"))
View(cerve)
cerve=cerve[,-1]####pre prosesamiento de datos "quitar una columna, fila-columna
colnames(cerve)=c("sexo","edad","p.p","500","1000","1500","2000","2500","3000","3500","4000","4500","5000")#cambiar nombre de las variables porque son muy largos
### gather=agrupar, crear variable precios "key=nueva", value=cantidades, todos los datos, despues las columnas que quiero que agrupe
df.long=gather(cerve,key="precios",value="cantidades","500","1000","1500","2000","2500","3000","3500","4000","4500","5000")##data frame pasar a formato long, estaban en formato white, queremos mas columnas que filas, queremos una columna que determine el precio, y lo que quiero qu me agrupe
#declarar variable precios y cantidades como numericos, con class se ve que es caracter, entonces se pasa a numerico
df.long$precios=as.numeric(df.long$precios)
df.long$cantidades=as.numeric(df.long$cantidades)
attach(df.long) ##volver a a tomar la nueva base de datos, base de datos reconocida por columnas como variables
names(df.long) #nombre variables
####
f.demanda=lm(as.numeric(cantidades)~as.numeric(precios))
summary(f.demanda)
##primer dato a"intercepto", abajo teta"negativo", pendiente negativa
### promedio 8-9 cervezas, 9 satisfechas "así sea gratis", por mas que quieran mas no pueden CMG=0, beta*1000, por cada 1000 pesos que aumente la cerveza dejan de consumir una cerveza y media -0.00146*1000pesos, 1.46 "dejan de consumir una cerveza y media
##ya con 9 queda satisfecho "punto saturación"
summary(f.demanda)
f.demanda$fitted.values ##regresion simple
df.long$prediccion=f.demanda$fitted.values ##regresion simple, agregar la columna a la tabla


##exportar a excel
library(openxlsx)
write.xlsx(df.long,"cerveexcel.xlsx")
write.xlsx(df.long,"cerveexcel2.xlsx", asTable = TRUE) #FORMATO TABLA FILTRO
