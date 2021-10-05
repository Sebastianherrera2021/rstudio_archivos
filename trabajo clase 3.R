library(readr)
delitos <- read_csv("C:/Users/ADMIN/Downloads/Delitos_en_Bucaramanga_enero_2010_a_febrero_de_2021.csv")
BARRIOS_HECHO
###suma de longitud
is.na(LONGITUD) #identificar valores nulos "NA"
sl=sum(LONGITUD,na.rm = T)
length(LONGITUD[is.na(LONGITUD)])  #identificar cuantos na
sum(is.na(LONGITUD)) #igual que el de arriba
length(LONGITUD) #cuantos valores hay en la tabla
Lon.lat= LONGITUD*LATITUD
sum(Lon.lat,na.rm = T) #sumar y no tener en cuenta ra
sl^2 ###altgr tecla despues de ñ y espacio
sl*sl  
lon.lat= LONGITUD*LATITUD
sum(lon.lat,na.rm = T)
mean(LONGITUD,na.rm = T) #media
sum(lon.lat^2,na.rm = T)
