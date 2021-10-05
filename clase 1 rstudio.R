#Nota, siempre siempre arreglar el simbolo del sistema chkdsk d: /f
#consola = simbolo del sistema
#objetos de r, vectores, matrices. data.frame, series 
a.1=c(23,45,6,7,8,9,9,0,8)
b=c(23,45,6,6,8,99,9,606,8)
c=b-a.1
d=b/2
rm(list = ls(a.1))
rm(a.1)
####VECTOR
promedio=c(2.5,4.2,3.0)
c=c(2,3,4)
promedio+c
equipo.c=c("Bucara","jaguares", "junior")
### Matrices
set.seed(5) ##semilla inicial, numeros aleatorios
d=rnorm(16,3.5,0.2) ##genera numeros con distribucion normal, media, desviacion estandar
m=matrix(d,4,4)
l=solve(m)
###para multiplicar matrices en R se debe colocar %*%
p=m%*%l
round(p,0)
###-2.664535e-15. -2.66*15 '-15. Notación cientifica
#Modificación de valores
p1=edit(p)
### 360=4x1-5.6x2+16x3, 560=9x1-8.6x2+11x3,100=5.6-2.6+30x3
g=c(360,560,100)
a=c(4,9,5.6,-5.6,-8.6,-2.6,16,11,30)
library(readr)
delitos <- read_csv("C:/Users/ADMIN/Downloads/Delitos_en_Bucaramanga_enero_2010_a_febrero_de_2021.csv")
