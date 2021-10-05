##```{r} 1.ANALISIS EXPLORATORIO DE LOS DATOS ```

install.packages('dplyr') #para manipular datos
install.packages('skimr') #para exploración inicial
install.packages('lubridate') #para manipular fechas
install.packages('tidyr') #para manipular datos
install.packages('ggplot2') #para hacer gráficos
#Cargar librerías
library(dplyr)
library(skimr)
library(ggplot2)
library(tidyr)
library(lubridate)
glimpse(mesysal)
skim(mesysal) 
knitr::kable(skim(mesysal))
#sd=desviacion tipica
#po=minimo
#p50 mediana
ggplot(mesysal,x=1) + geom_boxplot(aes(y=SALARIO))
#Caja borde superior percentil 25, mitad media y la otra percentil 
hist(MESES)
hist(SALARIO)
##rango intercuantilico 
### 
##Conclusiones:
# No hay nulos "Si hay nulos 815"
#Problemas con tipos de variables:
#- Measure2 y Measure3 también parecen más factores que enteros "Esto acá está bien"
# Viendo el mínimo y el p25 de Temperature parece que tiene atípicos

#4.Calidad de datos
#```{r}
#Corregimos los tipos de variables y los atípicos
#df <- df %>% "Simbolo para encadenar instrucciones una detras de otra"
# mutate(Measure2 = as.factor(Measure2), #Corregimos Measure2
#      Measure3 = as.factor(Measure3)) %>% #Corregimos Measure3 
# filter(Temperature > 50) #eliminamos los 4 atípicos de temperature "Los que están abajo"
#Todo esto se ahce si hay atipicos abajo


##
#5.Análisis exploratorio de variables (EDA)
#Exploramos las de tipo double aunque esta es para factor
mesysal %>%
  select_if(is.double) %>%
  gather() %>% #orden horizontal o vertical
  ggplot(aes(value)) + geom_bar() + facet_wrap(~key,scales='free') + #grafico de barras
  theme(axis.text=element_text(size=10))#esto es para cambiar el tamaño del texto del eje y que se lea bien "grafico por variable"

#Y las de tipo entero "si hubieran varios tipos de variables" "gRAFICO DE DENSIDAD"
mesysal %>%
  select_if(is.double) %>%
  gather() %>%
  ggplot(aes(value)) + geom_density() + facet_wrap(~key,scales='free') +
  theme(axis.text=element_text(size=10))#esto es para cambiar el tamaño del texto del eje y que se lea bien

#Hacemos análisis de correlaciones
mesysal %>%
  select_if(is.double) %>%
  cor() %>% 
  round(digits = 2) #redondear 
#lo bueno es que este cerca a 0 
#las meto en mi modelo de regresion sino estan correlacionadas
#si entre las mismas estan correlacionadas es normal


#Hacemos un zoom sobre el desbalanceo de la variable target
table(mesysal$failure) #variable de fallos yes or no, en la siguiente fase se corrige el ddesbalanceo

####
#Conclusiones:
# No se perciben patrones raros en las variables
#Las variables de medidas no correlacionan "entero""Es bueno"
#La variable target está muy desbalanceada

```{r} 2.6.Transformación de variables
No son necesarias grandes transformaciones porque el 
fichero ya viene muy limpio (no pasa así en la realidad)
Tampoco vamos a crear variables sintéticas 
(nuevas variables) que sí haríamos en la realidad 
(por ej número de fallos del mismo equipo, etc.)
Pero sí vamos a tener que trabajar sobre el balanceo
de la variable target ```
#FAILURE=TARGET"Destino"
#Vamos a pedirle si al agregar un nuevo dato al modelo va a ser un fallo o no
#el modelo busca que no falle 
#Que acierte el 99% de las veces, error bajisimo
#En la vida real no sirve para nada
#lo ideal un 50/50 "Dificil" o un 80/20
#como pasar de un 1% a un 80/20
#dos alternativas
#sobremuestreo:
##coger el total y si tengo pocos si, los replico x20 y aumento artificialmente en proporción 1%-10%

#inframuestreo:
#Lo contrario, elimino los no "aleatoriamente"y me queda en proporción el 80-20 
#Vamos a balancear usando la técnica del inframuestreo:
#Comprobamos la penetración exacta de la target
#Tenemos 81 sis que sobre el total de casos son un 0,9%:
81/nrow(df) * 100 #numero total de filas
#da 0.9 lo ideal sería subirlo a un 20% pero lo suben al 10


#Para tener casi un 10% necesitaríamos incrementar la proporción aprox en x10
#Entonces vamos a reducir los nos para que salga aprox esa proporción
#Nuevo df de nos
set.seed(1234) #para que nos salga lo mismo, es una semilla "extraccion aleatoria de los datos pero un poco falsa porque se tienen los mismos datos"

df_nos <- df %>% #nuevo conjunto de datos "los que no son fallos"
  filter(Failure == 'No') %>% #filtro 8699
  sample_frac(size = 0.08) #cree una muestra un funcion de una proporcion que sea del 0.08
8699*0.08 y variables
dim(df_nos)
#Df de sis
df_sis <- df %>% filter(Failure == 'Yes')
#Y los unimos de nuevo en un nuevo df reducido
df_red <- rbind(df_nos,df_sis)
#Comprobamos de nuevo la penetación de la target
count(df_red,Failure)
81/nrow(df_red) * 100
Ahora ya tenmos un dataset donde la target tiene un 10% de penetración (que sigue siendo poco pero lo dejaremos así)


#Df de sis
df_sis <- df %>% filter(Failure == 'Yes')
#Y los unimos de nuevo en un nuevo df reducido
df_red <- rbind(df_nos,df_sis)
#Comprobamos de nuevo la penetación de la target
count(df_red,Failure)
81/nrow(df_red) * 100
```
Ahora ya tenmos un dataset donde la target tiene un 10% de penetración (que sigue siendo poco pero lo dejaremos así)

#los datos no estaban balanceados entonces tocaba rebalancearlos
#reducimos eleatoriamente el conjunto de las mediciones que no represerntan
#un fallo de las maquinas, se crea un nuevo conjubnto de datos donde los que si
#representan pierden una proporcion cerca del 10% aunque no es lo ideal
#modelo machine learning
#hasta el momento se ha trabajado sobre las fases de un proyecto de 
#data science:
#Importacion, calidad de datos, transformacion, Modelización con evaluación
#falta ver modelización

REGRESION
#7.1 Dividir en entrentamiento y validación:
#No lo vamos a hacer por simplicidad y porque tenemos pocos casos
#COMPROBAR QUE EL MODELO FUNCIONE CON DATOS DIFERENTES A LOS QUE YA TENGA
#Partimos datos 70-30
#70 entrenamos este modelo
#el modelo lo validamos sobre el 30%
#se espera obtener una serie de medidas de evaluacion en el 30 similares
#a las del 70%, si son peores no da confianza
#7.2 Roles de las variables
target <- 'Failure' #variable objetivo "predecir"
indep <- names(df_red)[-20] #la variable 20 es Failure, variables predictoras "todas menos target"
#Coger el nombre de todas las variables y saque el de la posición 20
formula <- reformulate(indep,target) 
#pasarle cuales son los nombre de la variable independiente y targer
#construye una formula para desarrollar el modelo predictivo, predecir
#failure en funcion de todas las variables

Vamos a modelizar con una regresión logística
#regresion normal
#regresion multiple: predecir el salario en funcion de la edad y educación
#El modelo encuentra los pesos y coeficientes, v. cuantitativa
#Regresión logistica= salida entre 0: no hay fallos y 1: si hay fallos, v. targe
#variable lipotomica, "OJO" probabilidad de que se rompa la maquina
#tienen forma de S

rl <- glm(formula,df_red,family=binomial(link='logit'))
summary(rl) #Vemos el resultado
#FIJARSE EN EL ULTIMO VALOR Pr(>z)
#Indica si las variables son significativas o no de cara a predecir el fallo de una maquina
#da numeros y signos, indica nivel de significacion
#* 95% buen criterio
#Buscar las que tengan almenos un *
#eliminar las que no
Sólo resultan predictivas al menos al 95% tres variables, que vamos a seleccionar como finales

indep_fin <- c('Temperature','Humidity','Measure9') #significativas
formula <- reformulate(indep_fin,target) #actualizamos la fórmula

rl <- glm(formula,df_red,family=binomial(link='logit'))
summary(rl) #Vemos el resultado
#modelo bueno, en la vida real mucho mas pueba y error, requiere mirar
#mas metricas, residuos

Aplicamos nuestro modelo a los datos
df$scoring <- predict(rl,df,type='response') #apellido para probabilidad que se produzca la rutura de la mquina
#en modo ejecución"scoring"y no entrenamiento, calcular probabilidad de cada una de las variables
head(df$scoring) #ver primeros valores de la variable
##la medicion tiene un 12% de probabilidades de que la maquina falle.


Tomamos la decisión de si pensamos que será un fallo o no
#Como la penetración inicial era del 1%, vamos a poner un punto de corte muy alto, por ejemplo por encima del 80%
df$prediccion <- ifelse(df$scoring > 0.8,1,0)
head(df$prediccion)#0 y 1, antes no eran superior al 80%, ninguna se rompe
table(df$prediccion) #en total de conjuntos 8728-no"0", 52"1"-si
#como se si funciona o no?

Vamos a contrastar la predicción contra la realidad
table(df$prediccion,df$Failure)
No  Yes
0 8698   30
1    1   51
#primera fila predice si no falla
#segunda fila predice si falla
#1 colomuna realidad, veces que no falla y que si
#matriz de confusion para contrastar con la realidad

De todos los que predigo que van a fallar la mayoría fallan, pero también me estoy dejando muchos fallos en el tintero por ser tan conservador

Y si fueramos menos exigentes y pusiéramos el corte un poco más abajo?
  #hay una ciencia para esto
  Tomamos la decisión de si pensamos que será un fallo o no

#Vamos a ver qué pasa si bajamos la decisión al 60%
df$prediccion <- ifelse(df$scoring > 0.6,1,0) #menos conservador

Vamos a contrastar la predicción contra la realidad

table(df$prediccion,df$Failure)
No  Yes
0 8687   24
1   12   57


#menos exigentes, mas fallos reales pero hay mas falsos positivos
###GRAFICO DE PRECISION CONTRA COBERTURA
precision%>%
  ggplot(aes(Precision))+
  geom_line(aes(y=precision,color="precision"))+
  geom_line(aes(y=cobertura,color="cobertura"))+
  scale_color_discrete(name="metrica",labels=c("cobertura","precision"))
