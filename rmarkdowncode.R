##Henry Sebasti?n Rangel Qui?onez
##11 de julio de 2019
##Econometr?a I

####Semilla####
set.seed(1)

####Aleatorio####
hist(x)
x=rnorm(2000, 1030000, 200000)
beta1=rnorm(2000,0.3,0.01)
beta=runif(2000, 150000,2000000)

plot(density(beta1), col="Red")
####Ecuacion"####
Y=beta+beta1*x

####regresion Eficiencia ###
datos=data.frame(Y,x)
head(datos)
set.seed(1)
datos1=datos[sample(nrow(datos), 100), ] 
lm(Y~x, data= datos1)
set.seed(3)
datos2=datos[sample(nrow(datos), 100), ] 
lm(Y~x, data= datos2)
set.seed(4)
datos3=datos[sample(nrow(datos), 100), ] 

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
