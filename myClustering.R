#######Task 2-Clustering - Student Name: Shobhan Mitra,Student ID:45522156
#Import Packages with Library Command#
library("ggplot2") # Data visualization
library("plotly") # Interactive data visualizations
library("psych") # For correlation visualizations
library("caret") # Machine learning
library("party") # Decision Tree
library("class")
##Task2.1##
#load the preprocessed data file from Task1.
ilpd_df <- readRDS(file="ilpd_processed.Rda")
ilpd_df
#Exclude Age,Gender,AG_Ratio and Class attributes
myvars <- c("TB","DB","Alkphos","Sgpt","Sgot","TP","Albumin")
ilpd7col <- ilpd_df[myvars]
ilpd7col

##Task2.2##
#Rescale the values of every column to the range of (0,1) and convert datatype from vector to dataframe
ilpd7colScaled <- apply(ilpd7col,MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
ilpdF <- data.frame(ilpd7colScaled)

##Task2.3##
##***k=2 & Default Parameters***##
#cluster the data into 2 clusters(k=2) by K-Means clustering with default parameters)
ilpd_clusters <- kmeans(ilpdF[,-5], 2, nstart = 1)
ilpd_clusters
#AND plot the result of the clusters as 2D(X=Alkphos,Y=TP)
clusters_num <- as.factor(ilpd_clusters$cluster)
ggplot(ilpdF, aes(Alkphos, TP, color = clusters_num)) + geom_point()

##Task2.4##
#Plot another plot with same dimension but color the points according to class column
class_num <- as.factor(ilpd$Class)
ggplot(ilpdF, aes(Alkphos, TP, color = class_num)) + geom_point()
##***k=2 & changed hyper Parameters***##
#cluster the data into 2 clusters(k=2) by K-Means clustering with changed hyper parameters)
ilpd_clusters <- kmeans(ilpdF[,-5], 2, nstart = 25, iter.max=5)
ilpd_clusters
#AND plot the result of the clusters as 2D(X=Alkphos,Y=TP)Plot
clusters_num <- as.factor(ilpd_clusters$cluster)
ggplot(ilpdF, aes(Alkphos, TP, color = clusters_num)) + geom_point()


##Task2.6##
##***k=3 & changed hyper Parameters***##
#cluster the data into 3 clusters(k=3) by K-Means clustering with changed hyper parameters)
ilpd_clusters <- kmeans(ilpdF[,-5], 3, nstart = 25, iter.max=5)
ilpd_clusters
#AND plot the result of the clusters as 2D(X=Alkphos,Y=TP)Plot
clusters_num <- as.factor(ilpd_clusters$cluster)
ggplot(ilpdF, aes(Alkphos, TP, color = clusters_num)) + geom_point()
##***k=4 & changed hyper Parameters***##
#cluster the data into 4 clusters(k=4) by K-Means clustering with changed hyper parameters)
ilpd_clusters <- kmeans(ilpdF[,-5], 4, nstart = 25, iter.max=5)
ilpd_clusters
#AND plot the result of the clusters as 2D(X=Alkphos,Y=TP)Plot
clusters_num <- as.factor(ilpd_clusters$cluster)
ggplot(ilpdF, aes(Alkphos, TP, color = clusters_num)) + geom_point()
##***k=5 & changed hyper Parameters***##
#cluster the data into 5 clusters(k=5) by K-Means clustering with changed hyper parameters)
ilpd_clusters <- kmeans(ilpdF[,-5], 5, nstart = 25, iter.max=5)
ilpd_clusters
#AND plot the result of the clusters as 2D(X=Alkphos,Y=TP)Plot
clusters_num <- as.factor(ilpd_clusters$cluster)
ggplot(ilpdF, aes(Alkphos, TP, color = clusters_num)) + geom_point()

##Task 2.8## 
#Apply hierarchical Clustering to the data with default parameter & plot the corresponding Dendrogram
idx <- sample(1:nrow(ilpdF), 45)
distance_matrix <- dist(as.matrix(ilpdF[idx,-5]), method = "euclidean")
hc <- hclust(distance_matrix, method = "average")
plot(hc, hang = -5, labels = ilpd_df$Class[idx])

#Cluster the dendogram into 2 clusters
idx <- sample(1:nrow(ilpdF), 45)
distance_matrix <- dist(as.matrix(ilpdF[idx,-5]), method = "euclidean")
hc <- hclust(distance_matrix, method = "average")
plot(hc, hang = -5, labels = ilpd_df$Class[idx])
nclust <-  2
rect.hclust(hc,  k = nclust)

#Cluster the dendogram into 3 clusters
idx <- sample(1:nrow(ilpdF), 45)
distance_matrix <- dist(as.matrix(ilpdF[idx,-5]), method = "euclidean")
hc <- hclust(distance_matrix, method = "average")
plot(hc, hang = -5, labels = ilpd_df$Class[idx])
nclust <-  3
rect.hclust(hc,  k = nclust)

#Cluster the dendogram into 4 clusters
idx <- sample(1:nrow(ilpdF), 45)
distance_matrix <- dist(as.matrix(ilpdF[idx,-5]), method = "euclidean")
hc <- hclust(distance_matrix, method = "average")
plot(hc, hang = -5, labels = ilpd_df$Class[idx])
nclust <-  4
rect.hclust(hc,  k = nclust)

#Cluster the dendogram into 5 clusters
idx <- sample(1:nrow(ilpdF), 45)
distance_matrix <- dist(as.matrix(ilpdF[idx,-5]), method = "euclidean")
hc <- hclust(distance_matrix, method = "average")
plot(hc, hang = -5, labels = ilpd_df$Class[idx])
nclust <-  5
rect.hclust(hc,  k = nclust)

##Task2.10##
#"MIN"(ie,single) agglomeration method in Hierarchical Clustering
idx <- sample(1:nrow(ilpdF), 45)
distance_matrix <- dist(as.matrix(ilpdF[idx,-5]), method = "euclidean")
hc <- hclust(distance_matrix, method = "single")
plot(hc, hang = -5, labels = ilpd_df$Class[idx])
#"MAX"(ie,complete) agglomeration method in Hierarchical clustering
idx <- sample(1:nrow(ilpdF), 45)
distance_matrix <- dist(as.matrix(ilpdF[idx,-5]), method = "euclidean")
hc <- hclust(distance_matrix, method = "complete")
plot(hc, hang = -5, labels = ilpd_df$Class[idx])
#"AVERAGE" agglomeration method in Hierarchical clustering
idx <- sample(1:nrow(ilpdF), 45)
distance_matrix <- dist(as.matrix(ilpdF[idx,-5]), method = "euclidean")
hc <- hclust(distance_matrix, method = "average")
plot(hc, hang = -5, labels = ilpd_df$Class[idx])