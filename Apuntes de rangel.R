#####VECTOR 
promedio=c(2.5,4.2,3.0) #promedio de nota
c=c(2,3,4)
promedio+c
equipo.c=c("bucara", "jaguares",  "junior") #variable categorica
##Matrices 
set.seed(5) #Numeros aleatorios, fija los numeros aleatorios
d=rnorm(16,3.5,0.2)##genera numeros con distribución normal, datos, promedio y desviación.

m=matrix(d,4,4) 
l=solve(m) ###una matriz tiene inversa cuando es cuadrada
###para multiplicar matrices en R se debe colocar %*%
p=m%*%l #para que me de la matriz identidiad porcentajes
round(p,0)
###-1.110223e-16. -1.11 *10 ^ -16. notaciòn cientifica.  "exp" es el exponencial o euler
###modificacion de valores 
p1=edit(p)#cambiar manualmente numeros de matrices, solo quedan en la p1 y no en p
#trazabilidad de la investigación "lo más clara posible", encuesta datos originales y los cambios que use
#replicabilidad de la ciencia para llegar a los mismos resultados
#al final del articulo presentar codigos

### 360=4x1-5.6x2+16x3, 180=7x1-8.6x2+17x3, 500=4x1-5.6x2+112x3
g=c(360,180,500)
a=c(4,7,4,-5.6,-8.6,-5.6,16,17,12)
h=matrix(a,3,3)
solve(h,g) #hallar valores de X
