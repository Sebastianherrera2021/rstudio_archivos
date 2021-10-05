##Henry Sebasti?n Rangel Qui?onez
##11 de julio de 2019
##Econometr?a I

####Semilla####
set.seed(1)
##básicamente la función set.seed () ayudará a reutilizar el mismo conjunto de variables aleatorias,
##lo que podríamos necesitar en el futuro para evaluar nuevamente una tarea en particular nuevamente 
##con las mismas varibales aleatorias

####Aleatorio####
x=rnorm(2000, 1030000, 200000) ## n, media, desviación
x
hist(x=x)
#media <- mean(x) #media
#desv_est <- sd(x) #desviación estandar
#hist(x, main = "Edades", xlab = "Datos", ylab = "Frecuencia", col = "gold") #grafico solo
#abline(v = media, col = "red") #lineas rojas
#abline(v = media + (desv_est * c(1, -1)), col = "blue") # desviación azules


beta1=rnorm(2000,0.3,0.01) #pendiente## distribución normal: n, media y desviación, inventando valores 
#adapta una variable aleatoria a una función que depende de la media y la desviación típica.
beta=runif(2000, 150000,2000000) #distribución uniforme: n,media y desviación
#La distribución uniforme es una distribución continua que modela un rango de valores con igual probabilidad.

plot(density(beta1), col="Red") ##diagrama de densidad, pendiente, ##0.3 promedio "beta"
####Ecuacion"####
Y=beta+beta1*x ##genero variable y

####regresion Eficiencia ###
datos=data.frame(Y,x) #utilizan en R para almacenar datos en forma de hoja de datos.
#Datos corresponde a una observación o valor de una instancia, mientras que cada columna corresponde a 
#un vector que contiene los datos de una variable.
##asumiendo que no tenemos idea o indicios de la relacion entre x y y

head(datos)
set.seed(1)
datos1=datos[sample(nrow(datos), 100), ] #muestra de la poblacion, "revisar clase"
lm(Y~x, data= datos1)

set.seed(3)
datos2=datos[sample(nrow(datos), 100), ] 
lm(Y~x, data= datos2)

set.seed(4)
datos3=datos[sample(nrow(datos), 100), ] 
lm(Y~x, data= datos3)
##b11=-0.0585, b12=0.2938 "el que más se acerca al b real", b13=0.739 "duda explicacion"
efi=c(-0.0585,0.2938,0.739)
plot(efi)
sd(efi) #desviacion muestral

### Consisetencia 

set.seed(1)
datos12=datos[sample(nrow(datos), 10), ] 

set.seed(1)
datos13=datos[sample(nrow(datos), 1999), ] 
lm(Y~x, data= datos13)

### Insesgadez 
z=rnorm(2000,100000 , 20000 )
beta2=rnorm(2000,0.9,0.01)
l=beta+beta1*x+beta2*z
datos20=data.frame(l,x,z)
head(datos20)
set.seed(200)
datos21=datos20[sample(nrow(datos20), 1999), ] 
head(datos21)
summary(lm(l~z+x, data= datos21))
anova(lm(l~z+x, data= datos21))
ssto=(5.7974*(10^14))+(9.5845*(10^11))
R=(9.5845*(10^11))/ssto
