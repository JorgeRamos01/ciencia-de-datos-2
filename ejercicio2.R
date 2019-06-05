#################### Ejercicio 2#############
rm(list=ls())
source("../kkmedias.R")

#Generando los conjuntos de datos
#Las funciones usadas para generar dichos conjuntos se encuentran en
#http://data-mining-notebook.blogspot.mx/2015/02/kernel-k-means.html
set.seed(1000)   #Sinusoidal
N <- 200
sinusoidal <- function(m,noise=0.2) 
{
  x1 <- c(1:2*m)
  x2 <- c(1:2*m)
  for (i in 1:m) {
    x1[i] <- (i/m) * pi
    x2[i] <- sin(x1[i]) + rnorm(1,0,noise)
  }
  for (j in 1:m) {
    x1[m+j] <- (j/m + 1/2) * pi
    x2[m+j] <- cos(x1[m+j]) + rnorm(1,0,noise)
  }
  target <- c(rep(+1,m),rep(-1,m))
  return(data.frame(x1,x2,target))
}

dataset <- sinusoidal(N)
plot(dataset$x1,dataset$x2,col=as.factor(dataset$target), main="Sinusoide original", xlab="x", ylab="y")

new_dataset <- dataset[,1:2]
kmeans_obj <- kmeans(new_dataset, centers=2) #Aplicando k-means
plot(new_dataset, col=as.factor(kmeans_obj$cluster), main="kmeans", xlab="x",ylab="y")

#Aplicando kernel k-means
data_matrix <- as.matrix(new_dataset)
data_matrix<-GramMat(data_matrix,0.481) #0.481
b<-kkmedias1(data_matrix,2,iter=10)
plot(new_dataset,col=b, main="Kernel kmeans", xlab="x", ylab="y")

#Generando un par de anillos concentricos
make.ring <- function(R1, R2)
{
  v1 <- c()
  v2 <- c()
  for(i in 1:800)
  {
    t = 2*pi*runif(1,0,1)
    u = runif(1,0,1)+runif(1,0,1)
    if(u>1)
    {
      r = 2-u
    }
    else
    {
      r = u
    }
    if(r<R2)
    {
      r = R2+r*((R1-R2)/R2)
    }
    v1 <- c(v1, r*cos(t))
    v2 <- c(v2, r*sin(t))
  }
  return(as.matrix(data.frame(v1,v2)))
}
ring1 <- make.ring(8,5) 
ring2 <- make.ring(1,0)
plot(ring1, col="blue", main="Anillos conjunto original", xlab="x", ylab="y")
points(ring2, col="red")

#Aplicando kmeans
ring_dataset <- rbind(ring1, ring2)
kmeans_obj <- kmeans(ring_dataset, centers=2)
plot(ring_dataset, col=as.factor(kmeans_obj$cluster), main="Anillos con kmeans", xlab="x", ylab="y")

#Aplicando kernel kmeans
data_matrix1 <- as.matrix(ring_dataset)
data_matrix1<-GramMat(data_matrix1,0.05)
c<-kkmedias1(data_matrix1,2,iter=10)
plot(ring_dataset,col=c, main="Kernel kmeans", xlab="x", ylab="y")

#Probando con 3 anillos
ring11 <- make.ring(8,5) 
ring21 <- make.ring(1,0)
ring31 <- make.ring(20,12)
plot(ring31, col="blue", main="Anillos conjunto original", xlab="x", ylab="y")
points(ring2, col="red")
points(ring1, col="green")

#Aplicando kmeans
ring_dataset1 <- rbind(ring11, ring21,ring31)
kmeans_obj <- kmeans(ring_dataset1, centers=3)
plot(ring_dataset1, col=as.factor(kmeans_obj$cluster), main="Anillos con kmeans", xlab="x", ylab="y")

#Aplicando kernel kmeans
data_matrix11 <- as.matrix(ring_dataset1)
data_matrix11<-GramMat(data_matrix11,0.05)
c<-kkmedias1(data_matrix11,3,iter=10)
plot(ring_dataset1,col=c, main="Kernel kmeans,parametro=0.05", xlab="x", ylab="y")

#rm(data_matrix11)
