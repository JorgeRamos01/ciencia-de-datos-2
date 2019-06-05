############## Ejercicio 4 #########################
rm(list=ls())
rut.data<-"~/ciencia de datos/Tarea2/movie_reviews/txtfiles"
library(tm)
library(factoextra)
library("wordcloud")
diccionario<-c("films","film","movie","movies","one","characters","get","play", "stories","seem","two","look",
               "way","make","come","people","show","another","use","point","world","become",
               "find", "live", "will","story","around","looking","now", "man", "hes",
               "gets","every","things","say", "thing","part","something", "director","theres","going",
               "watch","making","seems","years","john","guy","family","goes","place","year","screen",
               "three","also")

corp <- Corpus(DirSource(rut.data,recursive=TRUE),
               readerControl=list(language="en_US"))
freqterms <- function(corp,n){
  
  ## si quieres leer los textos almacenados en la lista corp:
  ##inspect(corp[[1]])
  ## preproceso
  corp <- tm_map(corp,stripWhitespace)
  corp <- tm_map(corp,removeNumbers)
  corp <- tm_map(corp,content_transformer(tolower))
  corp <- tm_map(corp,removePunctuation)
  corp <- tm_map(corp,removeWords,diccionario)
  corp <- tm_map(corp,removeWords,stopwords("english"))
  #corp <- tm_map(corp,stemDocument)
  ## obtiene matriz de terminos
  tdm <- TermDocumentMatrix(corp,control=list(minDocFreq=100))
  ## remuevo terminos poco repetidos (95% sparsity)
  tdm <- removeSparseTerms(tdm, 0.95)
  ## frecuencia de terminos
  term.freq <- rowSums(as.matrix(tdm))
  sort.freq <- sort.int(term.freq,decreasing=TRUE,index.return=TRUE)
  ## los n mas frecuentes
  nterms.corp <- names(sort.freq$x[1:n])
  return(list(nterms=nterms.corp,tdm=tdm,ndocs=length(corp)))
}


count.terms <- function(tdms,terms){
  freq.table <- NULL
  for(i in 1:length(terms)){
    aa <- tm_term_score(tdms,terms[i])
    freq.table <- cbind(freq.table,aa)
  }
  colnames(freq.table) <- terms
  return(freq.table)
}

a<-freqterms(corp,50)          
c<-count.terms(a$tdm,a$nterms)   #Generamos la matriz los 50 términos más frecuentes
pca_doc<-princomp(c)            


fviz_eig(pca_doc, xlab="Componentes principales", ylab="Porcentaje de varianzas explicadas",geom="line")
z<-pca_doc$loadings[,1:3]
Z<-as.matrix(c) %*%z    #Proyectamos en 2 componentes
plot(Z[,1],Z[,3], col=c(rep('red',500),rep('blue',500)), pch =20, ylab="Tercera componente",xlab="Primera componente", main="3 componentes principales",ylim=c(-10,6))
legend(-22.5,-5,legend=c("Negativa","Positiva"), col=c("red","blue"),pch=19)
#distancias<- dist(Z)     
# set.seed(123)
# kfit <- kmeans(distancias, 2, nstart=100) #Establecemos los clusters usando k-means
# 
# #Graficamos los clusters para ver que tan bien clasifican nuestros datos.
# plot(kfit$cluster,col=c(rep('red',500),rep('blue',500)))
# legend(250,1.6,legend=c("Negativa","Positiva"),col=c("red","blue"), pch=19)
# table(kfit$cluster)
# library(cluster)
# clusplot(Z, kfit$cluster, color=T, shade=T, labels=2, lines=0)
# 
# #Midiendo la exactitud de la clasificación
# length(which(kfit$cluster[1:500]==2))
# length(which(kfit$cluster[501:1000]==1))


###########Inciso b#####

rut.data2<-"C:/Users/DELL/Documents/ciencia de datos/Tarea2/movie_revies2/txtfilesbloques"
corp2 <- Corpus(DirSource(rut.data2,recursive=TRUE),
               readerControl=list(language="en_US"))
a2<-freqterms(corp2,50)          
c2<-count.terms(a2$tdm,a2$nterms)   #Generamos la matriz los 50 términos más frecuentes
pca_doc2<-princomp(c2)   

fviz_eig(pca_doc2, xlab="Componentes principales", ylab="Porcentaje de varianzas explicadas",geom="line",main="Scree plot bloques=5")
z2<-pca_doc2$loadings[,1:3]
Z2<-as.matrix(c2) %*%z2    #Proyectamos en 2 componentes
plot(Z2[,2],Z2[,3], col=c(rep('red',100),rep('blue',100)), pch =20, xlab="Segunda componente",ylab="Tercer componente", main="2 componentes principales(bloques=5)",ylim=c(-12,11))
legend(-13.7,-5,legend=c("Negativa","Positiva"), col=c("red","blue"),pch=19)
# distancias2<- dist(Z2)     
# set.seed(123)
# kfit2 <- kmeans(distancias2, 2, nstart=100) #Establecemos los clusters usando k-means
# #Graficamos los clusters para ver que tan bien clasifican nuestros datos.
# plot(kfit2$cluster,col=c(rep('red',100),rep('blue',100)))
# legend(50,1.6,legend=c("Negativa","Positiva"),col=c("red","blue"), pch=19)
# table(kfit2$cluster)
# 
# clusplot(Z2, kfit2$cluster, color=T, shade=T, labels=2, lines=0)
# 
# #Midiendo la exactitud de la clasificación
# length(which(kfit2$cluster[1:100]==1))
# length(which(kfit2$cluster[101:200]==2))
