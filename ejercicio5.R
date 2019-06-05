rm(list=ls())
library(kernlab)
library(readr)

train_stock <- read_csv("~/ciencia de datos/Tarea2/train_stock.csv")
desc.x <- split(train_stock[,1], seq(nrow(train_stock)))
desc.y <- split(train_stock[,2], seq(nrow(train_stock)))

colors<-seq(1,length(desc.x))  #Generando etiquetas de color, azul=verdadero, rojo=falso
for (codigo in 1:length(desc.x)){  
  if (train_stock$same_security[codigo]=="TRUE"){colors[codigo]="blue"}
  else{colors[codigo]="red"}
}
#Generamos los String Kernels a usar
kern.spec1 <- stringdot(type="spectrum",length=2,  
                       normalized=TRUE)
kern.spec2 <- stringdot(type="boundrange",length=2,
                        normalized=TRUE)
kern.spec3 <- stringdot(type="constant",
                        normalized=TRUE)
kern.spec4 <- stringdot(type="exponential",length=2,
                        normalized=TRUE)

#Generamos la matriz de Gram para el kernel Spectrum 
#y graficamos en las primeras 2 componentes principales
ker_mat1<-kernelMatrix(kern.spec1, x=desc.x, y = desc.y)
kpc1 <- kpca(ker_mat1,kernel=kern.spec1,scale=c(), features=2)
plot(rotated(kpc1), col=colors,
     xlab="Primera Component",ylab="Segunda Componente", main="Spectrum")

#Generamos la matriz de Gram para el kernel Boundrange 
#y graficamos en las primeras 2 componentes principales
ker_mat2<-kernelMatrix(kern.spec2, x=desc.x, y = desc.y)
kpc2<- kpca(ker_mat2,kernel=kern.spec2,scale=c(), features=2)
plot(rotated(kpc2), col=colors,
     xlab="Primera Componente",ylab="Segunda Componente", main="Boundrange")

#Generamos la matriz de Gram para el kernel Constant 
#y graficamos en las primeras 2 componentes principales
ker_mat3<-kernelMatrix(kern.spec3, x=desc.x, y = desc.y)
kpc3<- kpca(ker_mat3,kernel=kern.spec3,scale=c(), features=2)
plot(rotated(kpc3), col=colors,
     xlab="Primera Componente",ylab="Segunda Componente", main="Constante")

#Generamos la matriz de Gram para el kernel Exponential 
#y graficamos en las primeras 2 componentes principales
ker_mat4<-kernelMatrix(kern.spec4, x=desc.x, y = desc.y)
kpc4<- kpca(ker_mat4,kernel=kern.spec4,scale=c(), features=2)
plot(rotated(kpc4), col=colors,
     xlab="Primera Componente",ylab="Segunda Componente", main="Exponential")


########Inciso b##################
#Entrenamos usando el kernel constante
ker_mat.train<-kernelMatrix(kern.spec3, x=desc.x)
kpc.train <- kpca(ker_mat.train,kernel=kern.spec3,scale=c(),features=2)
plot(rotated(kpc.train),xlab="Primer Componente",ylab="Segunda Componente", main="String Kernel Constant")

#Graficamos en las primeras 2 componentes principales resaltando los primeros 5 puntos
#de los conjuntos de entrenamiento y prueba para ver si coinciden
ker_mat.test<-kernelMatrix(kern.spec3, x=desc.y)
emb <- predict(kpc.train,ker_mat.test[1:5,])
points(emb,col="blue",pch=20)
points(rotated(kpc.train)[1:5,],col="red",pch=20)
#points(rotated(kpc.train)[1091,1],rotated(kpc.train)[1091,2],col="pink",pch=20)

#Entrenamos usando el kernel exponencial para ver si hace un mejor 
#trabajo que el kernel constantes
ker_mat.train1<-kernelMatrix(kern.spec4, x=desc.x)
kpc.train1 <- kpca(ker_mat.train1,kernel=kern.spec4,scale=c(),features=2)
plot(rotated(kpc.train1),xlab="Primer Componente",ylab="Segunda Componente", main="String Kernel Exponential")

#Graficamos en las primeras 2 componentes principales resaltando los primeros 5 puntos
#de los conjuntos de entrenamiento y prueba para ver si coinciden
ker_mat.test1<-kernelMatrix(kern.spec4, x=desc.y)
emb1 <- predict(kpc.train1,ker_mat.test1[1:5,])
points(emb1,col="blue",pch=20)
points(rotated(kpc.train1)[1:5,],col="red",pch=20)

#función que determina el indice cuya distancia euclideana
#es mínima de una columna de puntos a un punto
distancias<-function(columna,punto){
  n<-nrow(columna)
  dist<-rep(1,n)
  for (i in 1:n){
    dist[i]<-sqrt((columna[i,1]-punto[1])^2 + (columna[i,2]-punto[2])^2)
  } 
  which.min(dist)
}

#Vemos los textos que menor distancia tienen a los 5 puntos predecidos con kernel constante
desc.x[[distancias(rotated(kpc.train), emb[1,1:2])]]
desc.x[[distancias(rotated(kpc.train), emb[2,1:2])]]
desc.x[[distancias(rotated(kpc.train), emb[3,1:2])]]
desc.x[[distancias(rotated(kpc.train), emb[4,1:2])]]
desc.x[[distancias(rotated(kpc.train), emb[5,1:2])]]

#Vemos los textos que menor distancia tienen a los 5 puntos predecidos con kernel exponencial
desc.x[[distancias(rotated(kpc.train1), emb1[1,1:2])]]
desc.x[[distancias(rotated(kpc.train1), emb1[2,1:2])]]
desc.x[[distancias(rotated(kpc.train1), emb1[3,1:2])]]
desc.x[[distancias(rotated(kpc.train1), emb1[4,1:2])]]
desc.x[[distancias(rotated(kpc.train1), emb1[5,1:2])]]

#Puntos cercanos a los primeros 5 textos de dependent_y con el kernel constante
puntos_cerc<-c(distancias(rotated(kpc.train), emb[1,1:2]),
               distancias(rotated(kpc.train), emb[2,1:2]),
               distancias(rotated(kpc.train), emb[3,1:2]),
               distancias(rotated(kpc.train), emb[4,1:2]),
               distancias(rotated(kpc.train), emb[5,1:2]))

#Puntos cercanos a los primeros 5 textos de dependent_y con el kernel exponencial
puntos_cerc1<-c(distancias(rotated(kpc.train1), emb1[1,1:2]),
               distancias(rotated(kpc.train1), emb1[2,1:2]),
               distancias(rotated(kpc.train1), emb1[3,1:2]),
               distancias(rotated(kpc.train1), emb1[4,1:2]),
               distancias(rotated(kpc.train1), emb1[5,1:2]))


#Incluimos los 5 puntos mas cercanos con el string kernel constante en la gráfica
plot(rotated(kpc.train),xlab="1st Principal Component",ylab="2nd Principal Component", main="Zoom:String Kernel Constant",ylim=c(-5,11),xlim=c(-10,16))
points(emb,col="blue",pch=20)
points(rotated(kpc.train)[1:5,],col="red",pch=20)
points(c(rotated(kpc.train)[puntos_cerc[1],1],rotated(kpc.train)[puntos_cerc[2],1],
         rotated(kpc.train)[puntos_cerc[3],1],rotated(kpc.train)[puntos_cerc[4],1],
         rotated(kpc.train)[puntos_cerc[5],1]),
       c(rotated(kpc.train)[puntos_cerc[1],2],rotated(kpc.train)[puntos_cerc[2],2],
         rotated(kpc.train)[puntos_cerc[3],2],rotated(kpc.train)[puntos_cerc[4],2],
         rotated(kpc.train)[puntos_cerc[5],2]),col="orange",pch=20)

#Incluimos los 5 puntos mas cercanos con el string kernel exponencial en la gráfica
plot(rotated(kpc.train1),xlab="1st Principal Component",ylab="2nd Principal Component", main="Zoom:String Kernel Exponencial",ylim=c(-10,5),xlim=c(-10,20))
points(emb1,col="blue",pch=20)
points(rotated(kpc.train1)[1:5,],col="red",pch=20)
points(c(rotated(kpc.train1)[puntos_cerc1[1],1],rotated(kpc.train1)[puntos_cerc1[2],1],
         rotated(kpc.train1)[puntos_cerc1[3],1],rotated(kpc.train1)[puntos_cerc1[4],1],
         rotated(kpc.train1)[puntos_cerc1[5],1]),
       c(rotated(kpc.train1)[puntos_cerc1[1],2],rotated(kpc.train1)[puntos_cerc1[2],2],
         rotated(kpc.train1)[puntos_cerc1[3],2],rotated(kpc.train1)[puntos_cerc1[4],2],
         rotated(kpc.train1)[puntos_cerc1[5],2]),col="orange",pch=20)
