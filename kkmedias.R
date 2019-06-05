############ Matriz de Gram hecha en R 
#Función que aplica el producto interno con kernel gaussiano a dos vectores
# GaussKern.func<-function(x1,x2,parametro){ 
#   differ<-as.numeric(x1)-as.numeric(x2)
#   dist_transf<-t(differ)%*%differ 
#   dist_transf<-exp(-parametro*dist_transf)
#   dist_transf
# }
# 
# kernel_mat<-function(matriz,parametro=0.05){
#   n<-nrow(matriz)
#   gramMatr<-matrix(rep(0,n),nrow = n,ncol = n)
#   for(i in 1:n){ #Aprovechando la simetria de la matriz de kernel calculamos la parte triangular superior
#     for(j in i:n){
#       gramMatr[i,j]<-GaussKern.func(matriz[i,],matriz[j,],parametro)
#     }
#   }
#   for(i in 1:n){ #Generamos la parte triangular inferior usando la simetria de la matriz
#     for(j in 1:i){
#       gramMatr[i,j]<-gramMatr[j,i]
#     }
#   }
#   gramMatr
# }

############Matriz de Gram hecha en Rcpp
library('Rcpp')
library('inline')
library('RcppArmadillo')

cppFunction("arma::mat GramMat(arma::mat A, double parametro){
            arma::mat resultado(A.n_rows,A.n_rows);
            double temp;
              for (int i=0; i<A.n_rows; i++){
              for (int j=i; j<A.n_rows; j++){ //calculamos la parte triangular inferior de la matriz
                temp=  (double) -1*parametro*dot(A.row(i)-A.row(j),A.row(i)-A.row(j));
                resultado(i,j)=  exp(temp);
              }
              }
for (int i=0; i<A.n_rows; i++){   //Aprovechando la simetría de la matriz copiamos las demás entradas
  for (int j=0; j<i; j++){
    resultado(i,j)=resultado(j,i);
  }
}

            return resultado;}",depends="RcppArmadillo") 


kkmedias1<-function(kernel_mat,NumClust,iter){
  n<-nrow(kernel_mat)
  # Asignamos de manera aleatoria los elementos a los clusters
  set.seed(123)
  initClusters <- sample(NumClust, n, replace=TRUE)
  dist<-matrix(rep(0,n*NumClust),ncol=NumClust,byrow=T)
  pesos<-rep(1,n)
  t<-0
  #Calculamos el tercer sumando
  while(t<iter){
    tercTerm<-rep(0,NumClust)
    sum_pes3<-rep(0,NumClust) 
    for(j in 1:NumClust){ 
      for(h in which(initClusters==j)){
        sum_pes3[j]<-sum_pes3[j]+pesos[h]
        for(l in which(initClusters==j)){
          tercTerm[j]<-tercTerm[j]+pesos[h]*pesos[l]*kernel_mat[h,l]
        }
      }
    }
    for(i in 1:n){ #Generamos los otros 2 sumandos y calculamos las distancias
      primTerm<-0
      segTerm<-0
      sum_pes2<-0
      primTerm<-primTerm + kernel_mat[i,i]
      for(j in 1:NumClust){
        segTerm<-0
        sum_pes2<-0
        for(m in which(initClusters==j)){
          segTerm<-segTerm-pesos[m]*kernel_mat[m,i]
          sum_pes2<-sum_pes2+pesos[m]
        }
        dist[i,j] <- primTerm + 2*segTerm/sum_pes2 + tercTerm[j]/sum_pes3[j]^2 
      }
    }
    #Posible cambio en la asignación para elegir de manera aleatoria entre todos
    #los puntos que tienen m?nima distancia a un cluster para su reasignación.
    #initClusters<-apply(dist,1,function(x){sample(which(x==min(x)),1)})
    initClusters<-apply(dist,1,which.min)
    t<-t+1
  }
  initClusters
}
