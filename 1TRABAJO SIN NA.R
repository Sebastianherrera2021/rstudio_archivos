library(readxl) ##leer
datos<-(read_excel("C:Users/ADMIN/Downloads/hsranqui_Datos punto 2.xlsx"))
#PIB municipal variable dependiente
colnames(datos)=c("ciudad","habitantes 2019","Indice pobreza municipal 2018","PIB municipal 2018")#cambiar nombre de las variables porque son muy largos
attach(datos)
names(datos) #nombre variables

##regresión lineal #1, x=`habitantes 2019`, y=`PIB municipal 2018`
f.demanda=lm(as.numeric(`PIB municipal 2018`)~as.numeric(`habitantes 2019`)) #regresión lineal
summary(f.demanda)
plot(`habitantes 2019`,`PIB municipal 2018`)

##regresión lineal #2, x=`Indice pobreza municipal 2018`, y=`PIB municipal 2018`
f.demanda2=lm(as.numeric(`PIB municipal 2018`)~as.numeric(`Indice pobreza municipal 2018`)) #regresión lineal
summary(f.demanda2)
plot(`Indice pobreza municipal 2018`,`PIB municipal 2018`)

