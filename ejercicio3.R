#########Ejercicio 3################
rm(list=ls())
library(jpeg)
library(factoextra)
library(kernlab)
setwd("~/ciencia de datos/Tarea2/data_fruits_tarea")

source("../kkmedias.R")

files <- list.files(path=".", pattern=".jpg",all.files=T, full.names=F, no.. = T) 
list_of_images = lapply(files, readJPEG)
fruits<-as.data.frame(matrix(nrow = 1,ncol = 3))
for (i in 1:length(list_of_images)){
  fruits[i,1]<-median(list_of_images[[i]][,,1])
  fruits[i,2]<-median(list_of_images[[i]][,,2])
  fruits[i,3]<-median(list_of_images[[i]][,,3])
}
rm(list_of_images) #Eliminamos la lista de imagenes para ahorrar espacio en memoria
##############Inciso a
plot(fruits[,1],col="red",ylab="RGB escalado",xlab="Índice", main="Medianas: rojo vs verde")
points(fruits[,2],col="green")
plot(fruits[,3],col="blue",ylab="RGB escalado",xlab="Índice", main="Medianas: azul vs verde")
points(fruits[,2],col="green")
plot(fruits[,1],col="red",ylab="RGB escalado",xlab="Índice",main="Medianas: rojo vs azul")
points(fruits[,3],col="blue")

##############Inciso b
colores<-c(rep("red",100),rep("blue",100), rep("green",100),rep("pink",100),rep("orange",100), rep("grey",100),
           rep("black",100), rep("yellow",100),rep("purple",100),rep("brown",100), rep("cyan",100), 
           rep("magenta",100),rep("#0066CC",100))

#Esto solo se usa para construir más rápido una tabla en LATEX
#df_frutas<-data.frame(cbind(frutas<-c("Apple Braeburn", "Apple Golden", "Apple Granny Smith", "Apricot", "Avocado", "Carambula", "Cherry","Huckleberry","Kiwi","Orange", "Peach","Pineapple","Strawberry"),
#c("red","blue","green","pink","orange","grey","black","yellow","purple","brown","cyan","magenta","#0066CC")))
#colnames(df_frutas)<-c("Frutas", "Colores")

#knitr::kable(df_frutas, format = "latex", booktabs = T) 

#Realizamos PCA sobre la matriz de medianas
pca <- prcomp(fruits, center=TRUE, scale=TRUE)
fviz_eig(pca, xlab="Componentes principales", ylab="Porcentaje de varianzas explicadas",geom="line")
plot(pca$x[,1:2], xlab="Primer componente principal",ylab="Segunda componente principal", main="Primeras 2 componentes principales",col=colores)
library(scatterplot3d)
scatterplot3d(x=pca$x[,1],y=pca$x[,2],z=pca$x[,3],color=colores,angle=150, main="Componentes principales", sub="Representación en el espacio de las 3 componentes principales")

#Realizamos kernel PCA con kernel gaussiano a los datos
kern_pca<-kpca(~.,data=fruits, kernel="rbfdot",
               kpar=list(sigma=0.06),features=2) 
plot(rotated(kern_pca),xlab="Primer Componente",ylab="Segunda Componente", main="Kernel gaussiano", col=colores)

############Inciso c
#Aplicamos kmeans a los datos con PCA
set.seed(123)
kmeans_gen<-kmeans(pca$x, centers=13, iter.max = 10, nstart = 2)
table(kmeans_gen$cluster)
plot(kmeans_gen$cluster,col=colores, main="Cluster con k-medias RGB",xlab="Índice", ylab="Cluster")

#Aplicando kernel kmeans
pca_kernelMatrix<-GramMat(pca$x, 2.5) #2.5 es bastante decente
k_kmedias<-kkmedias1(pca_kernelMatrix,13,iter=10)
table(k_kmedias)
plot(k_kmedias, col=colores, main="Kernel k-means RGB", xlab="Índice", ylab="Clusters")
scatterplot3d(x=pca$x[,1],y=pca$x[,2],z=pca$x[,3],color=colores,angle=70, main="Kernel k-means", sub="Representación en el espacio de las 3 componentes principales")

#############Inciso d
library(imager)
files1 <- list.files(path=".", pattern=".jpg",all.files=T, full.names=F, no.. = T) 
list_of_images1 = lapply(files1, load.image)
fruits1<-as.data.frame(matrix(nrow = 1,ncol = 9))

#Construyendo la matriz de 1300x9 para almacenar los cuantiles centrales de la transformación HSV
for (i in 1:length(list_of_images1)){ 
  a<-RGBtoHSV(list_of_images1[[i]])
  fruits1[i,1:9]<-c(quantile(a[,,,1])[2:4],quantile(a[,,,2])[2:4],quantile(a[,,,3])[2:4])
}
rm(list_of_images1) #Eliminamos la variable en memoria para tener mas capacidad para trabajar

library(scatterplot3d)
#Relaciones HSV por cuantil
scatterplot3d(x=fruits1[,1],y=fruits1[,4],z=fruits1[,7],color=colores,angle=100, main="Percentil 25 HSV")
scatterplot3d(x=fruits1[,2],y=fruits1[,5],z=fruits1[,8],color=colores,angle=100, main="Percentil 50 HSV")
scatterplot3d(x=fruits1[,3],y=fruits1[,6],z=fruits1[,9],color=colores,angle=100, main="Percentil 75 HSV")

#Relación de las variables Hue en sus distintos cuantiles
scatterplot3d(x=fruits1[,1],y=fruits1[,2],z=fruits1[,3],color=colores,angle=70, main="Percentil 25 Hue")

#Realizando PCA sobre la matriz de frutas en HSV
pca1 <- prcomp(fruits1, center=TRUE, scale=TRUE)
fviz_eig(pca1, xlab="Componentes principales", ylab="Porcentaje de varianzas explicadas",geom="line")
scatterplot3d(x=pca1$x[,1],y=pca1$x[,2],z=pca1$x[,3],color=colores,angle=210, main="PCA",xlab="Primera componente", ylab="Segunda componente",zlab="Tercera componente")

kern_pca1<-kpca(~.,data=fruits1, kernel="rbfdot",
               kpar=list(sigma=0.03),features=2) 
plot(rotated(kern_pca1),xlab="Primer Componente",ylab="Segunda Componente", main="Kernel gaussiano", col=colores)

#Aplicamos kmeans a los datos con PCA
set.seed(123)
kmeans_gen2<-kmeans(pca1$x, centers=13, iter.max = 10, nstart = 2)
table(kmeans_gen2$cluster)
plot(kmeans_gen2$cluster,col=colores, main="Clusters con k-medias HSV",xlab="Índice", ylab="Cluster")

#Aplicando kernel kmeans
pca_kernelMatrix2<-GramMat(pca1$x, 0.5) #0.5 es bastante decente
k_kmedias2<-kkmedias1(pca_kernelMatrix2,13,iter=10)
table(k_kmedias2)
plot(k_kmedias2, col=colores, main="Kernel k-means HSV", xlab="Índice", ylab="Clusters")
scatterplot3d(x=pca1$x[,1],y=pca1$x[,2],z=pca1$x[,3],color=colores,angle=70, main="Kernel k-means", sub="Representación en el espacio de las 3 componentes principales")
