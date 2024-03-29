##Henry Sebasti?n Rangel Qui?onez
##11 de julio de 2019
##Econometr?a I

####Semilla####
set.seed(1)
##b�sicamente la funci�n set.seed () ayudar� a reutilizar el mismo conjunto de variables aleatorias,
##lo que podr�amos necesitar en el futuro para evaluar nuevamente una tarea en particular nuevamente 
##con las mismas varibales aleatorias

####Aleatorio####
x=rnorm(2000, 1030000, 200000) ## n, media, desviaci�n
x
hist(x=x)
#media <- mean(x) #media
#desv_est <- sd(x) #desviaci�n estandar
#hist(x, main = "Edades", xlab = "Datos", ylab = "Frecuencia", col = "gold") #grafico solo
#abline(v = media, col = "red") #lineas rojas
#abline(v = media + (desv_est * c(1, -1)), col = "blue") # desviaci�n azules


beta1=rnorm(2000,0.3,0.01) #pendiente## distribuci�n normal: n, media y desviaci�n, inventando valores 
#adapta una variable aleatoria a una funci�n que depende de la media y la desviaci�n t�pica.
beta=runif(2000, 150000,2000000) #distribuci�n uniforme: n,media y desviaci�n
#La distribuci�n uniforme es una distribuci�n continua que modela un rango de valores con igual probabilidad.

plot(density(beta1), col="Red") ##diagrama de densidad, pendiente, ##0.3 promedio "beta"
####Ecuacion"####
Y=beta+beta1*x ##genero variable y

####regresion Eficiencia ###
datos=data.frame(Y,x) #utilizan en R para almacenar datos en forma de hoja de datos.
#Datos corresponde a una observaci�n o valor de una instancia, mientras que cada columna corresponde a 
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
##b11=-0.0585, b12=0.2938 "el que m�s se acerca al b real", b13=0.739 "duda explicacion"
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
