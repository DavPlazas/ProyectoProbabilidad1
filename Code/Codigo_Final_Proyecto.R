#Codigo para cargar el shp

install.packages("rgdal") #Instalación de paquetes para usar archivos .shp
library(rgdal) #Incluir librerias
library(sp) #Incluir librerias


loc <- readOGR(file.choose()) #Se escoge el archivo localidades.shp disponible en GitHub
plot(loc)#Se grafica el mapa
box() #Se traza un borde para el mapa de calor


# Código para mapa de calor muertes y casos por localidad.


datos2 <- read.csv(file.choose(), header=T, sep=";", dec=",", row.names=1) #Se escoge el archivo Numero_de_casos_x_localidad.csv donde se tienen los datos
loc@data <- datos2 #Se cargan los datos a loc

spplot(loc,"Casos.En.Miles.",main = "Casos por localidad", ylab = "Casos en miles",col.regions=heat.colors(100,alpha=1,rev=T)) # Se grafica el mapa de calor de casos por localidad
spplot(loc,"Muertes",main = "Muertes por localidad", col.regions=heat.colors(100,alpha=1,rev=T))# Se grafica el mapa de calor de fellecidos por localidad


#Gráficos de pastel


install.packages("lessR") #Instalar paquete para graficas de pastel
library(lessR) #Llamado a la libreria lessR


#Pie Casos por sexo
#Se importa el archivo Casos_sexo.csv disponible en GitHub


ncxs = table(Casos_sexo) #Se carga el archivo Casos_sexo.csv a un dataframe
PieChart(ncxs, hole = 0, main = "Porcentaje de casos por sexo") #Se realiza un gráfico de pastel con la proporción de casos entre mujeres y hombres


#Pie Muertos por sexo
#Se importa el archivo Muertes_sexo.csv disponible en GitHub


nmxs = table(Muertes_sexo) #Se carga el archivo Casos_sexo.csv a un dataframe
PieChart(nmxs, hole = 0, main = "Porcentaje de muertes por sexo") #Se realiza un gráfico de pastel con la proporción de muertes entre mujeres y hombres


#Muertos por edad y sexo: hombre vs mujer
#Se importa el archivo ProyectoEdadesMuertos.csv disponible en GitHub


hist(ProyectoEdadesMuertos$EDAD_HOMBRE,breaks=10,main="Muertes por sexo",
     xlab="Edad",ylab="Frecuencia",col="#0080ff",alpha=0.5) #Se grafica un histograma de edades de los hombres fallecidos

hist(ProyectoEdadesMuertos$EDAD_MUJER, add=TRUE, breaks=10,main="Muertes por sexo",
     xlab="Edad",ylab="Frecuencia",col=rgb(1,0,0, alpha=0.5))#Se grafica un histograma de edades de las mujeres fallecidas

legend(x = "topright", legend = c("Hombre", "Mujer"), 
       fill = c("#0080ff", rgb(1,0,0)), title = "Sexo") #Se crea la convención de la gráfica


#Barplot numero de Muertos por Localidad
#Se importa el archivo Muertes_por_localidad disponible en el GitHub

ni = table(Muertes_por_localidad) #Se carga el archivo Muertes_por_localidad.csv a un dataframe
barp2 <- barplot(ni,main="Muertes por localidad", xlab="Código Localidad",ylab="Frecuencia", col="#dd99ff") #Se realiza el gráfico de barras con los datos
text(barp2, ni - 30, labels = ni)#Se añaden los valores numéricos a cada barra


#Promedio de edad de los muertos por localidad
#Se importa el archivo promedio_edad_muertes_localidad disponible en el GitHub

Id_loc<-promedio_edad_muertos_localidad$ID_LOCALIDAD #Se carga el id de las localidades a una variable
Prom_edad<-promedio_edad_muertos_localidad$EDAD_PROMEDIO_MUERTOS #Se carga el promedio de la edad de los fallecidos de las localidades a una variable

barp<-barplot(Prom_edad, names=Id_loc, main = "Promedio de edad de muertos por localidad", xlab="Código Localidad", ylab="Promedio de edad fallecidos", col=rgb(1,0,0)) #Se crea un gráfico de barras para el promedio de edades de fallecios por cada localidad

text(barp, Prom_edad - 0.9, labels = Prom_edad) #Se añaden los valores numéricos a cada barra


#Tiempo diagnostico por localidad
#Se importa el archivo TiempoDiagnostico disponible en GitHub


barp<-barplot(height=TiempoDiagnostico$Tiempo_promedio_d,name=TiempoDiagnostico$id_localidad,main="Tiempo Promedio Diagnostico por Localidad",xlab="Codigo Localidad",ylab="Dias transcurridos",col="#cc99ff") #Se realiza el gráfico de barras 
text(barp+0.05, TiempoDiagnostico$Tiempo_promedio_d -0.1, labels = TiempoDiagnostico$Tiempo_promedio_d) #Se añaden los valores numéricos a cada barra
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray") #Se dibujan las rectas horizontales para facilitar la visualización


#Regresion lineal: 
#Numero de casos (covid-positivo) acumulados por 
#días transcurridos desde el primer caso de covid en Bogotá
#Se importa el archivo Casos_acumulados_por_fecha disponible en GitHub

x <- Casos_acumulados_por_fecha$Dias_Transcurridos #Se asigna una variable a los dias transcurridos
y <- Casos_acumulados_por_fecha$Casos_acumulados #Se asigna una variable al numero de casos acumulados
plot(x,y,main="Casos acumulados por días transcurridos", 
     xlab = "Número de días transcurridos", ylab = "Número de casos acumulados") #Se realiza la grafica de dispersión de puntos
mod2 = lm(y~x) #Se modela una regresión lineal
abline(mod2, col = "red", lwd = 2) #Se traza la recta del modelo en la gráfica de dispersión


xb<-mean(x) #Hallamos la media de los dias transcurridos
yb<-mean(y) #Promedio de casos acumulados
Sxx<-sum((x-xb)^2) 
Sxy<-sum((x-xb)*(y-yb))        
Syy<-sum((y-yb)^2)        
b1<-Sxy/Sxx #Calculamos Beta 1
b0<-yb-b1*xb #Calculamos Beta 0

r<-b1*sqrt(Sxx/Syy) #correlación

rsquared=r^2 #r^2 es el coeficiente de determinación
#porcentaje de varianza en y causada en x


vl_n<-data.frame(x=450) #predicción para el Numero de casos al dia de hoy
predict(mod2,vl_n) #Se predicen los casos acumulados para el dia 450 (31/05/2021)

#intervalo de confianza con predict
predict(mod2,vl_n,level=0.95,interval="confidence")

#intervalo de prediccion con predict
predict(mod2,vl_n,level=0.95,interval="prediction")


#TEST DE HIPOTESIS sobre B1
#Dias transcurridos casos acumulados  
n=length(x)

B1_0=0 #H0
SSE = Syy - (b1*Sxy)
S = sqrt(SSE/(n-2))
C11 = 1/Sxx

T=(b1 - B1_0)/(S*sqrt(C11)) #Se usa el estimador T, pues la varianza no es conocida a primera vista.

alpha = 0.05
T_alpha=qt(alpha/2,lower.tail = FALSE,df=n-2)
#T=96.61 y T_alpha=1.96 -> rechazamos H_0
#Debido a que el estimador se encuentra en la región de rechazo, rechazamos la hipótesis nula.
#Luego, B1 es distinto de 0. Por lo tanto, verificamos que el número de casos acumulados depende del número de días transcuridos.









#Regresion lineal: 
#Numero de muertes acumuladas por edad en Bogotá
#Se importa el archivo Edad_y_fallecidos disponible en GitHub



x1 <- Edad_y_fallecidos$Edades
y1 <- Edad_y_fallecidos$Num_muertes
plot(x1,y1,main="Casos acumulados por días transcurridos", 
     xlab = "Número de días transcurridos", ylab = "Número de casos acumulados")
mod3 = lm(y1~x1)

mod3$coefficients

abline(mod3, col = "red")
summary(mod3)

## 

xb1<-mean(x1)
yb1<-mean(y1)
Sxx1<-sum((x1-xb1)^2)
Sxy1<-sum((x1-xb1)*(y1-yb1))        
Syy1<-sum((y1-yb1)^2)        
b1_n<-Sxy1/Sxx1
b0_n<-yb1-b1_n*xb1

r_n<-b1_n*sqrt(Sxx1/Syy1) #correlacion

rsquared_n = r_n^2 #r^2 es el coeficiente de determinacion
#porcentaje de varianza en y causada en x


#TEST DE HIPOTESIS sobre B1
n1=length(x1)

B1_0=0 #H0
SSE = Syy1 - (b1_n*Sxy1)
S = sqrt(SSE/(n1-2))
C11 = 1/Sxx1

T=(b1_n - B1_0)/(S*sqrt(C11))

alpha = 0.05
T_alpha=qt(alpha/2,lower.tail = FALSE,df=n1-2)
#T=5.93 y T_alpha = 1.98
#Debido a que el estimador se encuentra en la región de rechazo, rechazamos la hipótesis nula.
#Luego, B1 es distinto de 0. Por lo tanto, verificamos que el número de fallecidos depende de la edad.

 

