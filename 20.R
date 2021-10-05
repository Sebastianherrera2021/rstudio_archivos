##TRANSFORMACIONES PERO LA QUE HABIA HECHO YO NO EL PROFESOR################


urlfile.death<-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
muertes.total<-read.csv(urlfile.death)

urlfile.confir<-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirmados.total<-read.csv(urlfile.confir)

urlfile.recove<-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
recuperados.total<-read.csv(urlfile.recove)

###
attach(confirmados.total)
names(confirmados.total)
unique(confirmados.total$Country.Region) ###nombre de paises

#Puede ser de interés del investigador ???ltrar un solo país para analizar los datos, por lo que deberán aplicar lassiguientes líneas
confirmados.col=confirmados.total[Country.Region=="Colombia",]
confirmados.brazil=confirmados.total[Country.Region=="Brazil",]


####transformando de filas a columnas
#dataframe variables confirmados
#conficol=contagiados de colombia
##quitando primeras cuatro columnas


##La columna País.Región 
#contiene los nombres de los paises, para este caso se hace el ???ltro para Colombia
#,pero podrá ser modi???cado por cualquier otro país de la base de datos.
#Ahora, debemos modificar la base dedatos de horizontal a vertical y eliminar las que corresponden a caracteres así como,
#eliminar los valorescorresponde a una longitudy latitud
confi.col=data.frame(t(confirmados.col)[-c(1:4)])
confi.brazil=data.frame(t(confirmados.brazil)[-c(1:4)])

colnames(confi.col)=c("Col.confirmados")
colnames(confi.brazil)=c("brazil.confirmados")

attach(confi.col)
attach(confi.brazil)

##como serie de tiempo
#no es linea recta, exponencial,
confi.col.ts=ts(as.numeric(Col.confirmados),start=c(2020,1,22) , freq=365 )
confi.brazil.ts=ts(as.numeric(brazil.confirmados),start=c(2020,1,22) , freq=365 )

plot(confi.col.ts)
plot(confi.brazil.ts)

###cambios diarios 
plot(diff(confi.col.ts))
plot(diff(confi.brazil.ts))

