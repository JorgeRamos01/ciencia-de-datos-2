#########Ejercicio 1 ############
rm(list=ls())
genes <- read.csv("~/ciencia de datos/Tarea2/gene_expression_2classes.csv", header=FALSE)
gen_t<-t(genes)
rownames(gen_t)<-c(rep("Sano",20),rep("Enfermo",20))
colnames(gen_t)<-1:1000
set.seed(114) #Fijamos una semilla para obtener el mismo resultado usando k-means
kmeans_gen<-kmeans(gen_t, centers=2, iter.max = 10, nstart = 2)
table(kmeans_gen$cluster) #Contamos cuantos elementos hay en cada cluster
plot(kmeans_gen$cluster,col=c(rep("blue",20),rep("red",20)))  #Verificamos si estan correctamente identificados en cada cluster

#Preparamos el terreno y aplicamos los 3 métodos de clustering jerarquizados
#con medida de disimilaridad euclideana
distancias<-dist(gen_t, method="euclidean")
clus_sing <- hclust(d = distancias, method = "single") #Usando Single Linkage
clus_comp <- hclust(d = distancias, method = "complete") #Usando Complete Linkage
clus_aver <- hclust(d = distancias, method = "average") #Usando Average Linkage

#Graficando los dendogramas para la medidad de disimilaridad euclideana
library("factoextra")
fviz_dend(clus_sing, k=2, cex = 0.5, main="Dendograma enlace simple. Disimilaridad: euclideana")
fviz_dend(clus_comp, k=2, cex = 0.5, main="Dendograma enlace completo. Disimilaridad: euclideana")
fviz_dend(clus_aver, k=2, cex = 0.5,main="Dendograma enlace promedio. Disimilaridad: euclideana")

#Preparamos el terreno y aplicamos los 3 métodos de clustering jerarquizados
#con medida de disimilaridad correlación
dis_correl <- sqrt(1-cor(t(gen_t))^2)
dis_correl <- as.dist(dis_correl)
clus_sing1 <- hclust(d = dis_correl, method = "single") #Usando Single Linkage
clus_comp1 <- hclust(d = dis_correl, method = "complete") #Usando Complete Linkage
clus_aver1 <- hclust(d = dis_correl, method = "average") #Usando Average Linkage

#Graficando los dendogramas para la medidad de disimilaridad euclideana
fviz_dend(clus_sing1, k=2, cex = 0.5, main="Dendograma enlace simple. Correlación")
fviz_dend(clus_comp1, k=2, cex = 0.5, main="Dendograma enlace completo. Correlación")
fviz_dend(clus_aver1, k=2, cex = 0.5,main="Dendograma enlace promedio. Correlación")

#Aplicando PCA a los datos
pca<-prcomp(gen_t, scale=TRUE)
#Generamos un screeplot para valorar el número de componentes principales a usar
fviz_eig(pca, xlab="Componentes principales", ylab="Porcentaje de varianzas explicadas",geom="line")
plot(pca$x[,1], type="n",xlab="Componente principal 1",ylab="Componente principal 2", main="Primer componente principal")
text(pca$x[,1], labels = as.factor(rownames(gen_t)), col =as.numeric(as.factor(rownames(gen_t))))
legend(1,2,legend=c("Cluster 1", "Cluster 2"),col=c("red", "black"), lty=1, cex=0.8)

#Clusterizando usando la primera componente principal con kmeans
set.seed(114)
kmeans_gen1<-kmeans(pca$x[,1], centers=2, iter.max = 10, nstart = 3)
table(kmeans_gen1$cluster)
kmeans_gen1$cluster

#Clustering jerarquico con medida de disimilaridad euclideana con PCA
clus_sing2 <- hclust(d = dist(pca$x[,1]), method = "single") #Usando Single Linkage
clus_comp2 <- hclust(d = dist(pca$x[,1]), method = "complete") #Usando Complete Linkage
clus_aver2 <- hclust(d = dist(pca$x[,1]), method = "average") #Usando Average Linkage

#Graficando los dendogramas para la medidad de disimilaridad correlación 
#a los primeros 2 componentes principales
fviz_dend(clus_sing2, k=2, cex = 0.5, main="Dendograma enlace simple. Primer comp.")
fviz_dend(clus_comp2, k=2, cex = 0.5, main="Dendograma enlace completo.Primer comp.")
fviz_dend(clus_aver2, k=2, cex = 0.5,main="Dendograma enlace promedio. Primer comp.")


###############Inciso b #########################
library(ComplexHeatmap)
Heatmap(gen_t, na_col = "orange")
a<-Heatmap(gen_t[21:40,], na_col = "orange")
a
#El siguiente comando se puede utilizar para dibujar un rectangulo para aislar las filas
#y columnas correspondientes a dicho cuadro
#selectArea(mark = TRUE)

#Dado que el rectángulo "rojo" se encuentra en las últimas columnas del Heatmap
#se pueden extraer las últimas 50 columnas usadas en el heatmap de la siguiente manera
tail(column_order(a),50)

distancias1<-dist(genes[,21:40], method="euclidean")
clus_ <- hclust(d = distancias1, method = "complete")
b<-rect.hclust(clus_, k = 2, which = NULL, x = NULL, h = NULL,
            border = 2, cluster = NULL)
b[[1]] #Retorna los genes más representativos para el tejido enfermo
