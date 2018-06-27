#################################################################
# Primera parte
#################################################################

setwd("C:/Users/VEW0307/Downloads")

data=read.table("measure.txt")

X=data[,1:2]
n=dim(X)[1]
S=var(X)

#Spectral decomposition (finding eigenvalues and eigenvectors)
eigen(S)

#Solucion a rpimer omponente principal
w=eigen(S)$vectors[,1]
lambda=eigen(S)$values[1]

#New variable: First Component
medias=cbind(rep(colMeans(X)[1],n),rep(colMeans(X)[2],n))
XX=as.matrix(X-medias)  #datos centrados
Z=XX%*%w
plot(density(Z))

#Points reconstruction
ZZ=cbind(Z*w[1],Z*w[2])
ZZ=ZZ+medias  #agregar la medis otra vez
plot(ZZ)
plot(X,lwd=2)
points(ZZ,col=2,lwd=2)

#Using princomp

pp2=princomp(X)
summary(pp2,loadings=T)
sqrt(lambda)
lambda/sum(eigen(S)$values)


#Ahora construya el rpimer componente con las tres variables (3 to 1)

#################################################################
# Segunda parte
#################################################################

#De dos a dos dimensiones
X=data[,1:2]
plot(X)
pca=princomp(X,scores=T)
summary(pca,loadings=T)
pca$scores
par(mfrow=c(1,2))
plot(X)
plot(pca$scores)
par(mfrow=c(1,1))
biplot(pca)

#De tres a dos dimensiones

X=data[,1:3]
pca=princomp(X,scores=T)
summary(pca,loadings=T)
pca$scores
par(mfrow=c(1,2))
biplot(pca)
plot(pca$scores[,1],pca$scores[,2],col=c(2,4)[data[,4]])

#Grafico de codo
par(mfrow=c(1,1))
plot(pca$sdev^2,type="b",main="Varianzas de Componentes")


#Ahora cambiamos de base de datos

data=iris

X=data[,1:4]
pca=princomp(X,scores=T)
summary(pca,loadings=T)
pca$scores
par(mfrow=c(1,2))
biplot(pca)
plot(pca$scores[,1],pca$scores[,2],col=c(2,4,3)[data[,5]])




#################################################################
# Tercera parte
#################################################################

#######################################################
#data: handwritten digits

data=read.csv("digits.txt",head=F)
pix=data[,1:64]
y=data[,65]

par(mfrow=c(1,1))

##########################################
#PLOT

index=209

pixi=matrix(rep(0,64),ncol=8)
for(i in 1:8){
  for(j in 1:8){
    pixi[j,i]=pix[index,(i-1)*8+j]
  }
}
x=seq(0,1,by=1/8)
yy=seq(0,1,by=1/8)
image(x, yy, pixi,col = terrain.colors(100))
y[index]


##########################################
#Principal components

S=var(pix)
library(corrplot)
M=cor(pix)
corrplot(M, method="circle")

pp=princomp(pix,scores=T)
summary(pp)

#Grafico de codo
plot(pp$sdev^2,type="b",main="Varianzas de Componentes")


##############################################
#Images reconstruction
nn=dim(pix)[1]
p=dim(pix)[2]

#Solucion a rpimer omponente principal
w=eigen(S)$vectors

#New variable: First Component
medias = colMeans(pix)
pixx=as.matrix(pix - rep(medias, rep.int(nrow(pix), ncol(pix))))
Z=pixx%*%w

#Projections
#number of components
k=16
ZZ=Z%*%rbind(t(w)[1:k,],matrix(rep(0,(p-k)*p),ncol=p))
ZZ=as.matrix(ZZ+rep(medias, rep.int(nrow(pix), ncol(pix))))

index=209

pixi=matrix(rep(0,64),ncol=8)
pixi2=matrix(rep(0,64),ncol=8)

for(i in 1:8){
  for(j in 1:8){
    pixi[j,i]=pix[index,(i-1)*8+j]
    pixi2[j,i]=ZZ[index,(i-1)*8+j]
  }
}

x=seq(0,1,by=1/8)
yy=seq(0,1,by=1/8)

par(mfrow=c(1,2))
image(x, yy, pixi,col = terrain.colors(100))
image(x, yy, pixi2,col = terrain.colors(100))

y[index]




#################################################################
# Tercera parte
#################################################################


#########################################################################
#########################################################################
##Ejemplo Sencillo con un factor
##Datos de las flores de Iris (en paquete datasets)


##Carga de Paquetes
library(datasets)
library(MVA)


##Carga de Datos
x=iris
xx=x[,1:4]  #Usando las primeras 4 columnas


##Análisis factorial con datos iris
##factanal es la procedimiento con argumentos (datos, numero de factres)
fa=factanal(xx,1)
fa  ##Aca se ven los resultados del modelo estimado

##Pruebe que se puede hacer con solo la matriz de covarianza
S=var(xx)
fa2=factanal(covmat=S,n.obs=150,factors=1) 

fa3=factanal(xx,2)  ##Pruebe que con dos factores no se puede


##Analisis factorial calculando los scores (valores del factor por observacion)

fa=factanal(xx,1,scores="regression")
scor=fa$scores  ##Contiene el vector con el factor de cada flor (tamano)


##Visualizando los factores
plot(scor)  ##Dispesion del unico factor (no muy interesante)
plot(density(scor))  ##Desnsidad del factor (disribucion: mas util)


##Visualizando los factores por familia de flores (las tres de iris)
s1=scor[1:50]
s2=scor[51:100]
s3=scor[101:150]

plot(density(s1),xlim=c(-1.5,2),lwd=2)
lines(density(s2),lwd=2,col=2)
lines(density(s3),lwd=2,col=3)



#########################################################################
#########################################################################
##Real Example
##Holzinger ability test data (9 measures)
##Requiere paquete psych (con los datos)

library(psych)


##Cargando datos (matriz de corelaciones)
SS=Holzinger.9
cor.plot(SS)  ##Para visualizar la matriz de correlaciones


##Analisis Factorial

##Determinando el numero de factores a usar

factanal(factors=1,covmat=SS,n.obs=145)  ##Mirar el p-value de la prueba chi
factanal(factors=2,covmat=SS,n.obs=145)
factanal(factors=3,covmat=SS,n.obs=145)  ##Este parece ser...


##Analisis Factorial con tres factores

ff=factanal(factors=3,covmat=SS,n.obs=145)
ff  ##Interprete los factores obtenidos (con rotacion varimax)


##Cambiando la rotacion de los factores (promax)

ff2=factanal(factors=3,covmat=SS,n.obs=145,rotation="promax")
ff2  ##Interprete los factores obtenidos