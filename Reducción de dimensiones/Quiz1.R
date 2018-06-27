

#################################################################
# Quiz1
#################################################################

"""
Punto 1.  Componentes Principales

Utilizando el conjunto de datos "airquality" del paquete "datasets", 
construya la matriz de datos X usando las primeras cuatro variables: 
  nivel de ozono, radiación solar, viento y temperatura.
Información de los datos puede ser consultada ?airquality. 
(Use na.omit)

1.Suponga que se le pide construir un índice de severidad de clima
para el mes de mayo usando estas cuatro variables.
Encuentre la primra componente principal (con la matriz de varianza)
e interprétela.
2.Realice el biplot.
3.Encuentre las componentes principales usando la matriz de
correlaciones. Cuál matriz se debe usar para construir el índice,
S (varianzas) o R (correlaciones)?


Punto 2.  Análisis Factorial

Con el archivo de datos countries.csv, que tiene varios indicadores 
de desarrollo por países, use:
  
  d=read.table("countries.csv",sep=",",header=T, row.names="Countries")
d=na.omit(d)
d=d[,1:10]  #primeras 10 variables

1. Use un modelo factorial con dos factores
(use el parametro start=rep(0,10)).  
Interprete los dos factores resultantes.
2. Repita el análsis de factores usando la rotación Promax (u otra)
e interprete cada factor.
3. Recontruya el valor de los factores para cada país (scores).
Evalúe cómo le va a Colombia.

"""

setwd("C:/Users/VEW0307/Downloads")

library(datasets)
data("airquality")
air=na.omit(airquality[,1:4])
names(air)
n=nrow(air)

# Matriz de covarianza
S=round(var(air),2)
S

#Valores y vectores propios de la matriz de covarianza 
eigen(S)

# valor propio y vector - Primer componente principal
w=eigen(S)$vectors[,1]
lambda=eigen(S)$values[1]

#New variable: First Component
medias=cbind(rep(colMeans(air)[1],n),rep(colMeans(air)[2],n))
XX=as.matrix(air-medias)  #datos centrados
Z=XX%*%w
par(mfrow=c(1,1))
plot(density(Z))

#Points reconstruction
ZZ=cbind(Z*w[1],Z*w[2])
ZZ=ZZ+medias  #agregar la medis otra vez
plot(ZZ)



data=read.table("countries.csv", sep=",", dec=".", header=T)

head(data)
