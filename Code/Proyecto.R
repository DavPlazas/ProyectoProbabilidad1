#Codigo para cargar el shp
install.packages("rgdal")
library(rgdal)
library(sp)

loc <- readOGR(file.choose())
plot(loc)
box()

# Código para mapa de calor muertes y casos por localidad.
datos2 <- read.csv(file.choose(), header=T, sep=";", dec=",", row.names=1) 
loc@data <- datos2

spplot(loc,"Casos.En.Miles.",main = "Casos por localidad", ylab = "Casos en miles",col.regions=heat.colors(100,alpha=1,rev=T))
spplot(loc,"Muertes",main = "Muertes por localidad", col.regions=heat.colors(100,alpha=1,rev=T))

#Gráficos de pastel
install.packages("lessR")
library(lessR)
#Pie Casos por sexo
ncxs = table(Casos_sexo)
PieChart(ncxs, hole = 0, main = "Porcentaje de casos por sexo")

#Pie Muertos por sexo
nmxs = table(Muertes_sexo)
PieChart(nmxs, hole = 0, main = "Porcentaje de muertes por sexo")

#Muertos por edad y sexo: hombre vs mujer
hist(ProyectoEdadesMuertos$EDAD_HOMBRE,breaks=10,main="Muertes por sexo",
     xlab="Edad",ylab="Frecuencia",col="#0080ff",alpha=0.5)
     
legend(x = "topright", legend = c("Hombre", "Mujer"), 
       fill = c("#0080ff", rgb(1,0,0)), title = "Sexo")


hist(ProyectoEdadesMuertos$EDAD_MUJER, add=TRUE, breaks=10,main="Muertes por sexo",
     xlab="Edad",ylab="Frecuencia",col=rgb(1,0,0, alpha=0.5))

#Barplot numero de Muertos por Localidad
ni = table(Muertes_por_localidad)
barp2 <- barplot(ni,main="Muertes por localidad", xlab="Código Localidad",ylab="Frecuencia", col="#dd99ff")
text(barp2, ni - 30, labels = ni)

#Promedio de edad de los muertos por localidad

x<-promedio_edad_muertos_localidad$ID_LOCALIDAD
y<-promedio_edad_muertos_localidad$EDAD_PROMEDIO_MUERTOS

barp<-barplot(y, names=x, main = "Promedio de edad de muertos por localidad", xlab="Código Localidad", ylab="Promedio de edad fallecidos", col=rgb(1,0,0))

text(barp, y - 0.9, labels = y)

#Tiempo diagnostico por localidad

barp<-barplot(height=TimeD$Tiempo_promedio_d,name=TimeD$id_localidad,col="#cc99ff")
text(barp, TimeD$Tiempo_promedio_d -0.2, labels = TimeD$Tiempo_promedio_d)
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
#legend(x = "topleft", legend = c("-1: Sin dato", "1: Usaquen", "2: Engativa",
                                  #"3: Fontibon","4: Kennedy","5: Suba",
                                  #"6: Teusaquillo", "7: Chapinero", "8: Cuidad Bolivar",
                                  #"9: Barrios Unidos","10: Los Martires", "11: La candelaria",
                                 #"12: Rafael Uribe Uribe","13: Puente Arandano","14: Tunjuelito",
                                 #"15: Bosa","16: San Cristobal", "17: Sante Fe","18: Antonio Nariño",
                                 #"19: Usme", "20: Fuera de Bogota", "21: Sumapaz"
                                  #),
       #fill = c("#000066"), title = "Id_localidad",cex = 0.7)


#Regresion lineal: 
#Numero de casos (covid-positivo) acumulados por 
#dias transcurridos desde el primer caso de covid en Bogota

x<-prediccioncasos2$Dias_trancurridos
y<-prediccioncasos2$Cumulative_cases
plot(x,y,main="scatterplot",xlab="Numero de dias transcurridos",ylab="Numero de casos")

mod2=lm(y~x)
abline(mod2,col="red",lwd=2)
summary(mod2)
