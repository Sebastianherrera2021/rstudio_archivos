library(readxl)
mesysal <- read_excel("C:/Users/ADMIN/Downloads/mesesal.xlsx", col_types = c("numeric", "numeric"))
View(mesysal)
##Bo? y B1?, salarios:Bo+B1(mesestrabajo).   X(meses)=p6426, y(salario)=inglabo
attach(mesysal)
SAL.NA=sum(SALARIO,na.rm = T) #sumatoria sin tener en cuenta na
MES.NA=sum(MESES[!is.na(SALARIO)]) #is.na identifica valores nulos,y !is.na identifica los que no
#MES.NA=sum(MESES)-sum(MESES[is.na(SALARIO)]) #sumatoria de meses - sumatoria de meses donde el salario es nula
nm=length(MESES)-length(MESES[is.na(SALARIO)]) ## numero total de meses menos longitud de meses que tienen na en el salario #shift y tecla al lado ñ
Ns=length(SALARIO)-length(SALARIO[is.na(SALARIO)]) #DATOS TOTALES SIN NA
MULTIPLICACION=(MESES*SALARIO)
MULTI.NA=sum(MULTIPLICACION,na.rm = T)  #sumatoria de todos los que estan multiplicando menos na
MES_alcuadrado=MESES*MESES
MES_alcuadrado.NA=sum(MES_alcuadrado)-sum(MES_alcuadrado[is.na(SALARIO)]) ##NO CONTAR LOS MESES QUE TIENEN NA EN SALARIO
SALARIO_alcuadrado=SALARIO^2
SALARIO_alcuadrado.NA=sum(SALARIO_alcuadrado,na.rm = T)
B1=(MULTI.NA-(MES.NA*SAL.NA)/Ns)/((MES_alcuadrado.NA-((MES.NA)^2)/Ns))
B0=(SAL.NA/Ns)-B1*(MES.NA/Ns)
x<-MESES[!is.na(SALARIO)] 
y<-B1*x+B0
plot(x,y)
plot(MESES,SALARIO)

#FORMA DE BORRAR NA
datos <- na.omit(mesysal)


#ANALISIS EXPLORATORIO DE LAS VARIABLES
mesysal %>%
  select_if(is.double) %>%   #double=real
  gather() %>% #orden horizontal o vertical
  ggplot(aes(value)) + geom_bar() + facet_wrap(~key,scales='free') + #grafico de barras
  theme(axis.text=element_text(size=10))#esto es para cambiar el tamaño del texto del eje y que se lea bien "grafico por variable"
#DENSIDAD
mesysal %>%
  select_if(is.double) %>%
  gather() %>%
  ggplot(aes(value)) + geom_density() + facet_wrap(~key,scales='free') +
  theme(axis.text=element_text(size=10))
#CORRELACIONES
mesysal %>%
  select_if(is.double) %>%
  cor() %>% 
  round(digits = 2) #redondear
#   MESES SALARIO
#MESES    1.00    0.14
#SALARIO  0.14    1.00


names(mesysal)
sal=na.omit(SALARIO)
regresion<-lm(as.numeric(MESES) ~ as.numeric(sal)) #y,x , betas quedan en una misma unidad
summary(regresion) #b0=intercept y b1=pendiente , primer dato a"intercepto"

###este es
names(mesysal)
regresion<-lm(as.numeric(SALARIO) ~ (MESES)) #primero variable dependiente depues independiente (y,x)
summary(regresion) #b0 y b1

