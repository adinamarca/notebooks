
### Script R de analisis de BBDD "Chicago" por Alejandro Dinamarca.
## Docente: Francisco Cartes.
## Ramo: Estadistica Aplicada
### No se tildan palabras debido a que R no utiliza UTF-8.

### ___________________________________________
## Primera Sesion: 07/08/2020, 00:20 - 00:40 
## Comentarios: Realizacion de (0) a (3)
### ___________________________________________

### ___________________________________________
## Segunda Sesion: 07/08/2020, 20:00 - 21:30
## Comentarios: Realizaci贸n (4)
### ___________________________________________

### ___________________________________________
## Tercera Sesion: 11/08/2020, 00:15 - 02:16
## Comentarios: Realizaci贸n (5) y (6)
### ___________________________________________

### ___________________________________________
## Cuarta Sesion: 20/08/2020, 22:30 - 21/08/2020, 01:15
### ___________________________________________

### ___________________________________________
## Quinta Sesion: 21/08/2020, 15:08 - 17:00
### ___________________________________________

### ___________________________________________
## S閜tima Sesion: 21/08/2020, 21:08 - 24:00
### ___________________________________________

### ___________________________________________
## Quinta Sesion: 22/08/2020, 21:00 - 23/08/2020, 02:00
### ___________________________________________

### Extracto de clase 5:

#"Corresponden a datos de un estudio sobre la disponibilidad de seguros en Chicago 
#desde diciembre 1977 a febrero 1978. 
#Los datos estan dados para cada c?digo postal de Chicago (nombres de las filas 
#o casos). 
#Las variables son:
#race: la composicion racial en porcentaje minoritario
#fire: incendios por cada 100 unidades de vivienda
#theft: robo por cada 1000 habitantes
#age: porcentaje de las viviendas construidas antes de 1939
#volact: nuevas politicas de vivienda, ademas de las renovaciones menos cancelaciones y 
#no renovaciones por cada 100 unidades de vivienda
#involact: nuevas politicas del plan FAIR y renovaciones por cada 100 unidades de vivienda
#income: ingreso medio familiar"


### (0) Instalacion de paquetes y librerias necesarias

#install.packages("readr")
#install.packages("ppcor")
#install.packages("rel")
#install.packages('modeest')
#install.packages('ggplot2')
#install.packages('ggplus')
#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("hyper.fit")
#install.packages("plot3D")
#install.packages("psych")

### (1) Cargando paquetes y librerias instaladas

library(faraway)
library(dplyr)
library('prettyR')
library(readr)
library(ppcor)
library('modeest')
library(formattable)
library("tidyverse")
library("janitor")
library("hyper.fit")
library("plot3D")
library("psych")


#library('ggplot2')

### (2) Cargando BBDD "Chicago"

data(chicago)
chicago

### (3) Obtencion de medidas de tendencia central y medidas de dispersion a traves de ciclos iterativos


data <- data.frame(raza <- chicago$race, fuego <- chicago$fire, robo <- chicago$theft, edad <- chicago$age, politicaVivienda1 <- chicago$volact,
                         politicavivienda2 <- chicago$involact, ingresofamiliar <- chicago$income)

#summary(data)
#head(data)



dataMediaDF <- data.frame()

textoCol <- c("Composicion Racial", "Incendios por 100 Viviendas", "Robo por 1000 Habitantes", "Porcentaje Viviendas 1939", "Vieja Politica Vivienda", 
              "Nueva Politica Vivienda", "Ingreso Medio Familiar")
textoRow <- c("Promedio", "Mediana", "Moda unimodal", "Varianza", "Desviacion estandar", "CV", "Coef. Asimetria", "Curtosis")

n <- 1
for(val in data){
  dataMediaDF[1,n] <- round(mean(na.omit(data[,n])), digits=3)
  dataMediaDF[2,n] <- round(median(na.omit(data[,n])), digits=3)
  dataMediaDF[3,n] <- round(mfv(na.omit(data[,n]), na_rm=FALSE)[1], digits=3)
  dataMediaDF[4,n] <- round(var(na.omit(data[,n])), digits=3)
  dataMediaDF[5,n] <- round(sqrt(var(na.omit(data[,n]))), digits=3)
  dataMediaDF[6,n] <- round(dataMediaDF[5,n]/dataMediaDF[1,n], digits=3)
  dataMediaDF[7,n] <- round(skew((data[,n])), digits=3)
  dataMediaDF[8,n] <- round(kurtosi((data[,n])), digits=3)
  n <- n + 1
  if(n == 8){
    names(dataMediaDF) <- textoCol
    rownames(dataMediaDF) <- textoRow
  }
}

datamedidasDF <- data.frame(dataMediaDF)
datamedidasDF


# Dataframe de medidas de tendencia central y dispersi贸n con mejor apariencia visual

formattable(dataMediaDF, list('Composicion Racial' = color_tile("white", "orange"), 'Incendios por 100 Viviendas' = color_tile("white", "gray"),
                       'Robo por 1000 Habitantes' = color_tile("white", "pink"), 'Porcentaje Viviendas 1939' = color_tile("white", "cyan"),
                       'Vieja Politica Vivienda' = color_tile("white", "yellow"), 'Nueva Politica Vivienda' = color_tile("white", "pink"),
                       'Ingreso Medio Familiar' = color_tile("white", "gray")))



### (4) Plot antes de limpiar datos

# Ejecutar antes de plotear gr谩ficos


colores <- c("blue", "red", "green", "yellow", "blue", "red", "green", "yellow")
titulos <- c("Distribuci贸n Composici贸n Racial", "Distribuci贸n Incendios por 100 Viviendas", "Distribuci贸n Robo por 1000 Habitantes",
             "Distribuci贸n Porcentaje Viviendas 1939", "Distribuci贸n Vieja Politica Vivienda", "Distribuci贸n Nueva Politica FAIR",
             "Distribuci贸n Ingreso Medio Familiar")

nombresx <- c("Composici贸n racial en porcentaje minoritario", "Incendios cada 100 viviendas", "Robo cada 1000 habitantes", "Porcentaje viviendas construidas antes de 1939", "Viejas pol铆ticas de viviendas cada 100", "Nuevas pol铆ticas de viviendas cada 100","Ingreso medio familiar")
n <- 1
par(oma=c(1.5,1.5,1,1),mar=c(3.8,3.8,3.8,3.8),mfrow=c(2,2), col = 'white')


# Plot Histograma
par(mfrow=c(1,1))
n<-1
for (val in data){
  par(bg = 'black')
  hist(data[,n],freq=TRUE, ylab="Frecuencia", col=colores[n], main=titulos[n], xlab=nombresx[n], col.main="white", col.sub="white", col.lab="white",
       col.axis="white", fg="white")
  n <- n + 1
  plot.new
}

# Plot Densidad

n<-1
par(mfrow=c(2,2))
par(mfrow=c(1,1))
for (val in data){
  par(bg = 'black')
  list(plot(density(na.omit(data[,n])),main=titulos[n],ylab="Densidad",xlab=nombresx[n],las=1, col.main="white", col.sub="white", col.lab="white",
            col.axis="white", fg="white"),
       polygon(density(na.omit(data[,n])),col=colores[n]))
  n <- n + 1
  plot.new
}


# Plot Caja

n <-1
par(mfrow=c(2,2))
par(mfrow=c(1,1))
for (val in data){
  boxplot(data[,n], ylab="Frecuencia", col=colores[n], main=titulos[n], las=1, col.main="white", col.sub="white", col.lab="white", border="white",
          col.axis="white", fg="white")
  print(boxplot(data[,n], plot=FALSE)$out)
  n <- n + 1
}

iniciocor <- cor(data)
iniciocor

########################## Correlaciones simples antes de limpiar data ##########################


# La relaci贸n de RACE y FIRE es fuerte y positiva.
# La relaci贸n de RACE y THEFT es d茅bil y positiva.
# La relaci贸n de RACE y AGE es d茅bil y positiva.
# La relaci贸n de RACE y VOLACT es fuerte y negativa.
# La relaci贸n de RACE e INVOLACT es fuerte y positiva.
# La relaci贸n de RACE e INCOME es fuerte y negativa.

# La relaci贸n de FIRE y THEFT es fuerte y positiva.
# La relaci贸n de FIRE y AGE es moderada y positiva.
# La relaci贸n de FIRE y VOLACT es fuerte y negativa.
# La relaci贸n de FIRE e INVOLACT es fuerte y positiva.
# La relaci贸n de FIRE e INCOME es fuerte y negativa.

# La relaci贸n de THEFT y AGE es moderada y positiva.
# La realci贸n de THEFT y VOLACT es fuerte y negativa.
# La relaci贸n de THEFT e INVOLACT es moderada y positiva.
# La relaci贸n de THEFT e INCOME es fuerte y negativa.

# La relaci贸n de AGE y VOLACT es fuerte y negativa.
# La relaci贸n de AGE e INVOLACT es moderada y positiva.
# La relaci贸n de AGE e INCOME es fuerte y negativa.

# La relaci贸n de VOLACT e INVOLACT es fuerte y negativa.
# La relaci贸n de VOLACT e INCOME es fuerte y positiva.

# La relaci贸n de INVOLACT e INCOME es fuerte y negativa.


# Se observa tres outliers en la variable FIRE y THEFT, mientras en las variables AGE e INCOME solo un outlier

data

### (5) Limpiando datos con Tidyverse y R

clean_names(data)

outFire <- boxplot(data[,2], plot=FALSE)$out
outTheft <- boxplot(data[,3], plot=FALSE)$out
outAge <- boxplot(data[,4], plot=FALSE)$out
outIncome <- boxplot(data[,7], plot=FALSE)$out

data[which(data$fuego....chicago.fire %in% outFire),]
data[6,2] <- datamedidasDF[1,2]
data[23,2] <- datamedidasDF[1,2]
data[24,2] <- datamedidasDF[1,2]

#data <- data[-which(data$fuego....chicago.fire %in% outFire),]

# Se descarta limpiar datos THEFT dado que al limpiar datos de FIRE ya se eliminan filas comprometidas en THEFT

data[which(data$robo....chicago.theft %in% outTheft),]
data[6,3] <- datamedidasDF[1,3]
data[7,3] <- datamedidasDF[1,3]
data[24,3] <- datamedidasDF[1,3]

#data <- data[-which(data$robo....chicago.theft %in% outTheft),]



data[which(data$edad....chicago.age %in% outAge),]
data[37,4] <- datamedidasDF[1,3]

#data <- data[-which(data$edad....chicago.age %in% outAge),]

data[which(data$ingresofamiliar....chicago.income %in% outIncome),]
data[7,7] <- datamedidasDF[1,7]

#data <- data[-which(data$ingresofamiliar....chicago.income %in% outIncome),]

# Tras limpieza, se arrojan tres outlier m谩s, dos en FIRE y uno en THEFT

outFire <- boxplot(data[,2], plot=FALSE)$out
outTheft <- boxplot(data[,3], plot=FALSE)$out


data[which(data$fuego....chicago.fire %in% outFire),]
data[11,2] <- datamedidasDF[1,2]
data[34,2] <- datamedidasDF[1,2]


data[which(data$robo....chicago.theft %in% outTheft),]
data[5,3] <- datamedidasDF[1,3]
data[45,3] <- datamedidasDF[1,3]

# Tras nueva limpieza, se arrojan dos utliner m谩s, uno en FIRE y otro en THEFT

outFire <- boxplot(data[,2], plot=FALSE)$out
outTheft <- boxplot(data[,3], plot=FALSE)$out


data[which(data$fuego....chicago.fire %in% outFire),]
data[26,2] <- datamedidasDF[1,2]


data[which(data$robo....chicago.theft %in% outTheft),]
data[29,3] <- datamedidasDF[1,3]


# Termino de limpieza de outliers
#(total limpiados: 5 filas de datos)
#--> Tras repensar el m茅todo de limpieza y la p茅rdida de datos si se eliminan filas, se opt贸 por
# corregir dichos outlier colocando la media en las celdas correspondientes. Por lo tanto

# se corregieron 6 outlier de la variable FIRE

# se corregieron 6 outlier de la variable THEFT

# se corregieron 1 outlier de la variable AGE

# se corregieron 1 outlier de la variable INCOME


data

finalcor <- cor(data)
cor <- finalcor
cor


### (6) Plot despu茅s de limpiar datos

# Ejecutar antes de plotear gr谩ficos


colores <- c("blue", "red", "green", "yellow", "blue", "red", "green", "yellow")
titulos <- c("Distribuci贸n Composici贸n Racial", "Distribuci贸n Incendios por 100 Viviendas", "Distribuci贸n Robo por 1000 Habitantes",
             "Distribuci贸n Porcentaje Viviendas 1939", "Distribuci贸n Nueva Politica Vivienda", "Distribuci贸n Nueva Politica FAIR",
             "Distribuci贸n Ingreso Medio Familiar")

nombresx <- c("Composici贸n racial en porcentaje minoritario", "Incendios cada 100 viviendas", "Robo cada 1000 habitantes", "Porcentaje viviendas construidas antes de 1939", "Viejas pol铆ticas de viviendas cada 100", "Nuevas pol铆ticas de viviendas cada 100","Ingreso medio familiar")
n <- 1
par(oma=c(1.5,1.5,1,1),mar=c(3.8,3.8,3.8,3.8),mfrow=c(2,2))

# Plot Histograma

n<-1
for (val in data){
  par(bg = 'black')
  hist(data[,n],freq=TRUE, ylab="Frecuencia", col=colores[n], main=titulos[n], xlab=nombresx[n], col.main="white", col.sub="white", col.lab="white",
       col.axis="white", fg="white")
  n <- n + 1
  plot.new
}

# Plot Densidad

n<-1
par(mfrow=c(2,2))
for (val in data){
  par(bg = 'black')
  list(plot(density(na.omit(data[,n])),main=titulos[n],ylab="Densidad",xlab=nombresx[n],las=1, col.main="white", col.sub="white", col.lab="white",
            col.axis="white", fg="white"),
       polygon(density(na.omit(data[,n])),col=colores[n]))
  n <- n + 1
  plot.new
}

# Plot Caja

n <-1
par(mfrow=c(2,2))
for (val in data){
  boxplot(data[,n], ylab="Frecuencia", col=colores[n], main=titulos[n], las=1, col.main="white", col.sub="white", col.lab="white", border="white",
          col.axis="white", fg="white")
  n <- n + 1
}


# (7) Calculando correlaciones parciales y p.valor


cor
########################## Correlaciones simples tras limpieza de data  ##########################


# La relaci贸n de RACE y FIRE es fuerte y positiva.
# La relaci贸n de RACE y THEFT es fuerte y positiva.
# La relaci贸n de RACE y AGE es d茅bil y positiva.
# La relaci贸n de RACE y VOLACT es fuerte y negativa.
# La relaci贸n de RACE e INVOLACT es fuerte y positiva.
# La relaci贸n de RACE e INCOME es fuerte y negativa.

# La relaci贸n de FIRE y THEFT es fuerte y positiva.
# La relaci贸n de FIRE y AGE es moderada y positiva.
# La relaci贸n de FIRE y VOLACT es fuerte y negativa.
# La relaci贸n de FIRE e INVOLACT es fuerte y positiva.
# La relaci贸n de FIRE e INCOME es fuerte y negativa.

# La relaci贸n de THEFT y AGE es moderada y positiva.
# La realci贸n de THEFT y VOLACT es fuerte y negativa.
# La relaci贸n de THEFT e INVOLACT es moderada y positiva.
# La relaci贸n de THEFT e INCOME es fuerte y negativa.

# La relaci贸n de AGE y VOLACT es fuerte y negativa.
# La relaci贸n de AGE e INVOLACT es moderada y positiva.
# La relaci贸n de AGE e INCOME es fuerte y negativa.

# La relaci贸n de VOLACT e INVOLACT es fuerte y negativa.
# La relaci贸n de VOLACT e INCOME es fuerte y positiva.

# La relaci贸n de INVOLACT e INCOME es fuerte y negativa.


# Recordar: al restar la intersecci贸n de varianzas de ambas serie de datos A y B (en este caso, la intersecci贸n de las varianzas de
#las siete variables), se elimina el ruido de una variable C
# que afecta las correlaciones simples, por ende, la correlaci贸n parcial es m谩s segura.
# "Eliminando la varianza compartida por las variables de inter茅s con la o las variables auxiliares, obtenemos una medida de r
# que refleja los efectos de las variables de inter茅s primario."




corr <- pcor(data)$estimate
corr
########################## Correlaciones parciales ##########################



# Debido al ruido entre dos variables, constituido por las varianzas de otras 5 variables comprometidas en la base de datos, 
# la correlaci贸n simple puede no ser del todo certera.
# Es por ello que, tras eliminar la intersecci贸n de varianzas entre las variables, se modifican dichas correlaciones resultando en las
# correlaciones parciales que, en la pr谩ctica, son las correlaciones simples pero sin el ruido interferiendo entre variables.
# Por lo tanto, redefiniendo dichas correlaciones de manera m谩s segura ser谩 posible apreciar cambios:


# La relaci贸n de RACE y FIRE es d茅bil y positiva.
# La relaci贸n de RACE y THEFT es moderada y positiva.
# La relaci贸n de RACE y AGE es fuerte y negativa.
# La relaci贸n de RACE y VOLACT es d茅bil y negativa.
# La relaci贸n de RACE e INVOLACT es moderada y positiva.
# La relaci贸n de RACE e INCOME es moderada y negativa.

# La relaci贸n de FIRE y THEFT es casi nula y negativa.
# La relaci贸n de FIRE y AGE es casi nula y positiva.
# La relaci贸n de FIRE y VOLACT es d茅bil y negativa.
# La relaci贸n de FIRE e INVOLACT es moderada y positiva.
# La relaci贸n de FIRE e INCOME es casi nula y negativa.

# La relaci贸n de THEFT y AGE es d茅bil y positiva.
# La realci贸n de THEFT y VOLACT es casi d茅bil y negativa.
# La relaci贸n de THEFT e INVOLACT es casi d茅bil y negativa.
# La relaci贸n de THEFT e INCOME es casi nula y positiva.

# La relaci贸n de AGE y VOLACT es d茅bil y negativa.
# La relaci贸n de AGE e INVOLACT es d茅bil y positiva.
# La relaci贸n de AGE e INCOME es d茅bil y negativa.

# La relaci贸n de VOLACT e INVOLACT es d茅bil y negativa.
# La relaci贸n de VOLACT e INCOME es casi fuerte y positiva.

# La relaci贸n de INVOLACT e INCOME es casi nula y positiva.


pvalor <- pcor(data)$p.value
pvalor

########################## P valor ########################## 


# De aqu铆 se obtiene que aquellas relaciones estad铆sticamente significativas son aquellas con un p valor menor a 0.05:

# RACE y THEFT.
# RACE y AGE.
# RACE e INVOLACT.
# RACE e INCOME.

# FIRE y VOLACT.
# FIRE e INVOLACT.

# THEFT y FIRE.
# THEFT y VOLACT.
# THEFT e INVOLACT.

# VOLACT e INCOME.

# INVOLACT y FIRE.


# (8) Obteniendo gr谩ficos de dispersi贸n

print("######################### MODELO DE REGRESI覰 SIMPLE #########################")
######################### MODELO DE REGRESI覰 SIMPLE #########################

pairs(data, col.main="white", col.sub="white", col.lab="white",
      col.axis="white", fg="white", pch=20,col="red")


# RACE

# R cuadrado: 34.96%
rel<-lm(data$raza....chicago.race~+data$robo....chicago.theft)
rel
summary(rel)
x = data$robo....chicago.theft
y = data$raza....chicago.race
plot(x=x, y=y , xlab="Theft", ylab="Race", col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="yellow", pch=20, main="Modelo de regresi髇 simple RACE y THEFT")
abline(lm(y~x))


# R cuadrado: 5.473%
rel<-lm(data$raza....chicago.race~+data$edad....chicago.age)
rel
summary(rel)
x = data$edad....chicago.age
y = data$raza....chicago.race
plot(x=x, y=y , xlab="Age", ylab="Race", col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="purple", pch=20, main="Modelo de regresi髇 simple RACE y AGE")
abline(lm(y~x))


# R cuadrado: 50.94%
rel<-lm(data$raza....chicago.race~+data$politicavivienda2....chicago.involact)
rel
summary(rel)
x = data$politicavivienda2....chicago.involact
y = data$raza....chicago.race
plot(x=x, y=y , xlab="Involact", ylab="Race", col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="red", pch=20, main="Modelo de regresi髇 simple RACE e INVOLACT")
abline(lm(y~x))


# R cuadrado: 58.34%
rel<-lm(data$raza....chicago.race~+data$ingresofamiliar....chicago.income)
rel
summary(rel)
x = data$ingresofamiliar....chicago.income
y = data$raza....chicago.race
plot(x=x, y=y , xlab="Income", ylab="Race", col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="green", pch=20, main="Modelo de regresi髇 simple RACE e INCOME")
abline(lm(y~x))



# FIRE


# R cuadrado: 65.32%
rel<-lm(data$fuego....chicago.fire~+data$politicaVivienda1....chicago.volact)
rel
summary(rel)
x = data$politicaVivienda1....chicago.volact
y = data$fuego....chicago.fire
plot(x=x, y=y , xlab="Volact", ylab="Fire", col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="orange", pch=20, main="Modelo de regresi髇 simple FIRE y VOLACT")
abline(lm(y~x))



# R cuadrado: 61.1%
rel<-lm(data$fuego....chicago.fire~+data$politicavivienda2....chicago.involact)
rel
summary(rel)
x = data$politicavivienda2....chicago.involact
y = data$fuego....chicago.fire
plot(x=x, y=y , xlab="Involact", ylab="Fire", col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="brown", pch=20, main="Modelo de regresi髇 simple FIRE e INVOLACT")
abline(lm(y~x))

# THEFT 

# R cuadrado: 26.17%
rel<-lm(data$robo....chicago.theft~+data$fuego....chicago.fire)
rel
summary(rel)
x = data$fuego....chicago.fire
y = data$robo....chicago.theft

plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="blue", pch=20, ylab="Theft", xlab="Fire", main="Modelo de regresi髇 simple FIRE y THEFT")
abline(lm(y~x))


# R cuadrado: 45.27%
rel<-lm(data$robo....chicago.theft~+data$politicaVivienda1....chicago.volact)
rel
summary(rel)
x = data$politicaVivienda1....chicago.volact
y = data$robo....chicago.theft
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="tomato", pch=20, ylab="Theft", xlab="Volact", main="Modelo de regresi髇 simple VOLACT y THEFT")
abline(lm(y~x))

# R cuadrado: 15.56%
rel<-lm(data$robo....chicago.theft~+data$politicavivienda2....chicago.involact)
rel
summary(rel)
plot(rel)

x = data$politicavivienda2....chicago.involact
y = data$robo....chicago.theft

plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="magenta", pch=20, ylab="Theft", xlab="Involact", main="Modelo de regresi髇 simple INVOLACT y THEFT")
abline(lm(y~x))


# VOLACT

#  R cuadrado: 78.93%
rel<-lm(data$politicaVivienda1....chicago.volact~+data$ingresofamiliar....chicago.income)
rel
summary(rel)
plot(rel)

x = data$ingresofamiliar....chicago.income
y = data$politicaVivienda1....chicago.volact

plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="dark red", pch=20, ylab="Volact", xlab="Income", main="Modelo de regresi髇 simple INCOME y VOLACT")
abline(lm(y~x))

# INVOLACT

# R cuadrado: 51.4%
rel<-lm(data$politicavivienda2....chicago.involact~+data$ingresofamiliar....chicago.income)
rel
summary(rel)
plot(rel)

x = data$ingresofamiliar....chicago.income
y = data$politicavivienda2....chicago.involact

plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="cyan", pch=20, ylab="Involact", xlab="Income", main="Modelo de regresi髇 simple INVOLACT e INCOME")
abline(lm(y~x))


print("######################### MODELO DE REGRESI覰 M贚TIPLE #########################")
######################### MODELO DE REGRESI覰 M贚TIPLE #########################

print("######################### INVOLACT (nueva politica de viviendas) #########################")
######################### INVOLACT (nueva politica de viviendas) #########################

par(oma=c(1.5,1.5,1,1),mar=c(3.8,3.8,3.8,3.8),mfrow=c(3,3), col = 'white')
regresion <-(data$politicavivienda2....chicago.involact~data$raza....chicago.race+data$fuego....chicago.fire+data$robo....chicago.theft+data$edad....chicago.age+data$ingresofamiliar....chicago.income)

# R cuadrado del modelo con INCOME y el intercepto: 67.03%
rel<-lm(regresion)
rel

summary(rel)

pairs(regresion, pch = 16, cex = 1.3, col = "blue", main = "Involact con Income e Intercept", col.main="white", col.sub="white", col.lab="white", col.axis="white", fg="white")

# R cuadrado del modelo sin INCOME y el intercepto: 84.31%
regresionfix <- (data$politicavivienda2....chicago.involact~data$raza....chicago.race+data$fuego....chicago.fire+data$robo....chicago.theft+data$edad....chicago.age - 1)
rel<-lm(regresionfix)
rel
summary(rel)

### 1

x = data$raza....chicago.race
y = data$politicavivienda2....chicago.involact
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="dark red", pch=20, ylab="Involact", xlab="Race", main="Modelo de regresi髇 m鷏tiple INVOLACT, gr醘ico de RACE")
abline(rel)

### 2

x = data$fuego....chicago.fire
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="dark red", pch=20, ylab="Involact", xlab="Fire", main="Modelo de regresi髇 m鷏tiple INVOLACT, gr醘ico de FIRE")
abline(rel)

### 3

x = data$robo....chicago.theft
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="dark red", pch=20, ylab="Involact", xlab="Theft", main="Modelo de regresi髇 m鷏tiple INVOLACT, gr醘ico de THEFT")
abline(rel)

### 4

x = data$edad....chicago.age
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="dark red", pch=20, ylab="Involact", xlab="Age", main="Modelo de regresi髇 m鷏tiple INVOLACT, gr醘ico de AGE")
abline(rel)



x <- (data$fuego....chicago.fire+data$robo....chicago.theft+data$edad....chicago.age - 1)
y <- data$politicavivienda2....chicago.involact

pairs(regresionfix, pch = 16, cex = 1.3, col = "blue", main = "Involact sin Income e Intercept",col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white")


print("######################### VOLACT (vieja politica de viviendas) #########################")
######################### VOLACT (vieja politica de viviendas) #########################

par(oma=c(1.5,1.5,1,1),mar=c(3.8,3.8,3.8,3.8),mfrow=c(3,3), col = 'white')
regresion <-(data$politicaVivienda1....chicago.volact~data$raza....chicago.race+data$fuego....chicago.fire+data$robo....chicago.theft+data$edad....chicago.age+data$ingresofamiliar....chicago.income)


x <- data$raza....chicago.race+data$fuego....chicago.fire+data$robo....chicago.theft+data$edad....chicago.age+data$ingresofamiliar....chicago.income
y <- data$politicaVivienda1....chicago.volact

# R cuadrado del modelo con FIRE y el intercepto: 84.61%
rel<-lm(regresion)
rel

summary(rel)


pairs(regresion, pch = 16, cex = 1.3, col = "blue", main = "Modelo Volact c/ Race e Intercept", col.main="white", col.sub="white", col.lab="white", col.axis="white", fg="white")

plot(data$politicaVivienda1....chicago.volact, col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="cyan", pch=20, ylab="Frecuencia", xlab="Volact", main="Modelo de regresi髇 m鷏tiple VOLACT c/ Race e Intercept")
abline(rel)

# R cuadrado del modelo sin RACE y el intercepto: 95.38%
regresionfix <- (data$politicaVivienda1....chicago.volact~data$robo....chicago.theft+data$edad....chicago.age+data$fuego....chicago.fire+data$ingresofamiliar....chicago.income - 1)
rel<-lm(regresionfix)
rel
summary(rel)




pairs(regresionfix, pch = 16, cex = 1.3, col = "blue", main = "Modelo Volact s/ Race e Intercept",col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white")


### 1

x = data$robo....chicago.theft
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="cyan", pch=20, ylab="Volact", xlab="Theft", main="Modelo de regresi髇 m鷏tiple VOLACT, gr醘ico de THEFT")
abline(rel)


### 2

x = data$edad....chicago.age
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="cyan", pch=20, ylab="Volact", xlab="Age", main="Modelo de regresi髇 m鷏tiple VOLACT, gr醘ico de AGE")
abline(rel)


### 3

x = data$fuego....chicago.fire
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="cyan", pch=20, ylab="Volact", xlab="Fire", main="Modelo de regresi髇 m鷏tiple VOLACT, gr醘ico de FIRE")
abline(rel)

### 4

x = data$ingresofamiliar....chicago.income
plot(x=x, y=y , col.main="white", col.sub="white", col.lab="white",
     col.axis="white", fg="white",col="cyan", pch=20, ylab="Volact", xlab="Income", main="Modelo de regresi髇 m鷏tiple VOLACT, gr醘ico de INCOME")
abline(rel)



# R cuadrado del modelo sin FIRE, AGE y el intercepto: 62.23%, definitivamente, no es sugerente eliminar AGE al derivar en un modelo menos exacto

regresionfix <- (data$politicaVivienda1....chicago.volact~data$raza....chicago.race+data$fuego....chicago.fire+data$robo....chicago.theft - 1)
rel<-lm(regresionfix)
rel
summary(rel)





# ahora s铆, la 煤ltima pregunta: en involact la variable "income" no es significativa, por lo cual, se podr铆a inferir que gracias a la nueva pol铆tica de viviendas, no es relevante tener un bajo/alto ingreso familiar para acceder a ellas en comparaci贸n con la antigua... sin embargo, con la variable no significativa "fire" en volact se podr铆a inferir tambi茅n que estas nuevas viviendas son m谩s susceptibles a incendios que las anteriores...
# pareciera ser que la nueva pol铆tica de viviendas trajo consigo, viviendas m谩s accesibles pero m谩s susceptibles a incendios...

######################### Plano de regresi贸n para INVOLACT#########################


# *** Desde aqu铆 dado que la variable "age" no tiene mayor relevancia en nueva pol铆tica de viviendas (en t茅rminos de las edades de las personas que adquieren casas con INVOLACT)
# entonces, se remueve dicha variable para realizar un an谩lisis a mayor detalle con un plano de regresi贸n solo con race, fire y theft
# sin embargo, dado que dicha gr谩fica requerir铆a de 4 dimensiones, (con volact o involact en relaci贸n con race, fire y theft), se debe
# eliminar otra variable para realizar el plano, por lo cual, se opta por realizar tres relaciones distintas por sustituci贸n, para as铆
# quedar con aquellas gr谩ficas relevantes

# Primera relaci贸n: INVOLACT con fire y theft

par(mar=c(1,1,1,1), mfrow=c(1,1))
x <- data$robo....chicago.theft
y <- data$fuego....chicago.fire
z <- data$politicavivienda2....chicago.involact

# Realizando el plano de regresi贸n lineal (z = ax + by + d)

# Dispersi贸n 
open3d()
plot3d(x, y, z, back = "lines")


# Plano de regresi贸n 
fit <- lm(z ~ x + y)

# Gr谩fica 3D
open3d()
plot3d(fit, which=1, plane.col="orange", use_surface3d=TRUE)









