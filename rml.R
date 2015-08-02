###Sebastián Alcívar


##2.1 Leer los archivos poblacion1.xlsx y poblacion2.xlsx , y analizar sus dimensiones.

library(readxl)
poblacion1 <- read_excel("poblacion1.xlsx",col_names = TRUE, na="")
poblacion2 <- read_excel("poblacion2.xlsx",col_names = TRUE, na="")
str(poblacion1)
str(poblacion2)
dim(poblacion1)
dim(poblacion2)

##2.2 Una los archivos leídos en un mismo objeto llamado poblacion .
#No tengo igual número de filas, para no perder la informacion de poblacion 1
#aumento las filas que me faltan en poblacion2 con valores vacios

#verificamos los datos que nos faltan y aumentamos filas
orden <- order(poblacion2.1[,"identificador"])
poblacion2.1 <- poblacion2
poblacion2.1 <- rbind(poblacion2.1,data.frame(identificador=1024,part.almz.escl=NA,var.ingresos=NA,tasa.crimen=NA,var.tasa.crimen=NA,region=NA,serv.bas.compl=NA))
poblacion2.1 <- rbind(poblacion2.1,data.frame(identificador=1025,part.almz.escl=NA,var.ingresos=NA,tasa.crimen=NA,var.tasa.crimen=NA,region=NA,serv.bas.compl=NA))
poblacion2.1 <- rbind(poblacion2.1,data.frame(identificador=1030,part.almz.escl=NA,var.ingresos=NA,tasa.crimen=NA,var.tasa.crimen=NA,region=NA,serv.bas.compl=NA))
poblacion2.1 <- rbind(poblacion2.1,data.frame(identificador=1043,part.almz.escl=NA,var.ingresos=NA,tasa.crimen=NA,var.tasa.crimen=NA,region=NA,serv.bas.compl=NA))

#♣Unimos los archivos
poblacion <- merge(poblacion1,poblacion2.1)

##2.2 Cree un código que identifique la clase de cada variable y genere diagramas de cajas
##para variables continuas y diagramas de barras para variables discretas.

clase <- lapply(poblacion, class)
for (i in 1:length(clase)){
  if (clase[[i]]=="numeric"){
    boxplot(poblacion[,i],main=i)
  }
  else{
    barplot(table(poblacion[,i]),main=i)
  }
}

##2.3 Cree un código que calcule automáticamente el mínimo, media, máximo, desviación
##estándar, primer cuartil de cada variable numérica y la frecuencia en el caso de
##variables categóricas.

clase <- lapply(poblacion, class)
media <- c()
minimo <- c()
maximo <- c()
desviacion <- c()
cuartil <- c()
frec <- list()
for (i in 1:length(clase)){
  if (clase[[i]]=="numeric"){
    media[i] <- mean(poblacion[,i],na.rm=TRUE)
    minimo[i] <- min(poblacion[,i],na.rm=TRUE)
    maximo[i] <- max(poblacion[,i],na.rm=TRUE)
    desviacion[i] <- sd(poblacion[,i],na.rm=TRUE)
    cuartil[i] <- quantile(poblacion[,i], probs = seq(0, 1, 0.25), na.rm = TRUE)[2]
  }
  else {
    frec[[i]] <- table(poblacion[,i])
  }
}
media
minimo
maximo
desviacion
cuartil
frec

##2.4 Calcule la correlación entre la variable dependiente poblacion y cada una de las
##variables explicativas (numéricas).

correlacion <- c()
for(i in 1:6){
  correlacion[i]=cor(poblacion[,2],poblacion[,i+2],use="na.or.complete")
}

##2.5 Considere la variable categórica serv.bas.compl con una confiabilidad del 90%, ¿Pue-
##de asumirse que la media de la variable poblacion en el grupo serv.bas.compl: SI
##es distinta a la media del grupo serv.bas.compl: NO ?

pob_si <- subset(poblacion, subset=serv.bas.compl == 'SI')
pob_no <- subset(poblacion, subset=serv.bas.compl == 'NO')
t.test(pob_si[,2], pob_no[,2], alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)
#Entonces la diferencia de las medias es distinta de cero,lo que es igual a decir que
#las medias no son iguales

##2.6 Considerando los cálculos anteriores genere el modelo de regresión lineal múltiple que
##mejor se ajuste a los datos. Interprete los coeficientes obtenidos.

#solo tomamos los regresores numericos
reg_num<-poblacion[,clase=="numeric"]
#eliminamos el identificador de la tabla
regresores <- reg_num[,-1]
regresion<-lm(poblacion~.,regresores)
summary(regresion)
#elejimos los mejores regresores
reg<-step(regresion,direction="backward")
summary(reg)
#El modelo de regresión lineal que mejor se ajusta es:
#poblacion=8.617 - 0.012tasa.crimen
#El modelo solo depende de un regresor y su interpretación es:
#Por cada unidad que se aumenta een tasa.crimen, la poblacion disminuye en promedio 0.0125

##2.7 Interprete el R2.

summary(reg)["r.squared"]
#14.0567% es el porcentaje de la variabilidad explicada por la regresión con respecto a la
#variabilidad total
#Se puede apreciar que el porcentaje es bastante bajo y se puede decir qie la variabilidad
#explicada por la regresion no es "buena"

##2.8 Analice la significancia de la regresión y de cada uno de los parámetros individuales.

t_anova <- aov(reg)
summary(t_anova)
qf(0.95 , df1=1,df2=38)
#como el valor f=6.215 es mayor a 4.098, entonces la regresion es significativa

summary(reg)
qt(0.975 , df =38)
#El t de tasa.crimen, su valor absoluto es mayor que qt(0.975 , df =38)=2.024
#por lo que tomando a tasa.crimen individualmente si es significativo
#y se puede rechazar h0 donde el coeficiente es igual a cero
#se puede comprobar mediante el intervalo de confianza

confint(reg,level=0.95)

#donde el cero no pertenece al intervalo a un 95% de confianza

##2.9 Realice un análisis detallado de los residuos.
p<-merge(poblacion1,poblacion2)
residuo <- reg[["residuals"]]
prediccion <- reg[["fitted.values"]]
data <- data.frame(poblacion=p[,"poblacion"],tasa.crimen=p[,"tasa.crimen"], prediccion,residuo)
datatable(data,filter="top", options = list(searching = TRUE, pageLength = 5, lengthMenu = c(5, 10, 15)))


hist(residuo,15)
#Tiene cierta tendencia normal con media cero
mean(residuo)
#la media de los residuos es prácticamente cero, por lo que no viola las hipotesis de normalidad
qqnorm(residuo)
qqline(residuo,col="red")
#presenta linealidad y se puede considerar adecuado

plot(residuo,prediccion)
plot(residuo,data[,"tasa.crimen"])
#Al parecer los datos si se encuentran dentro de una franja en ambos casos, por lo que no se
#evidencia alguna violación de las hipotesis

plot(data[,"poblacion"],data[,"tasa.crimen"])
#Se observa una tendencia lineal prácticamente horizontal como lo muestra su coeficiente de
#regresion bastante cercano a cero

plot(residuo,data[,"poblacion"])

