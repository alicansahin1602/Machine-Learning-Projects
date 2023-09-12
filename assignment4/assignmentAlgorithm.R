#This homework is done by Ali Can Þahin 2018402189 and Selin Coþkun 2018402213 together. This is a group assignment.


#install.packages('cluster')
library(cluster)




setwd('C:/Users/alica/OneDrive/Masaüstü/ali belgeler/BOUN/3.Sýnýf/2.DÖNEM/IE 425/HW4')
data = read.csv('EastWestAirlines.csv')
data
str(data)
data = data[,-1]

data = scale(data[-1])
data

center = c(1.441145e+02,2.059515e+00,1.014504e+00,1.012253e+00,1.714485e+04,1.160190e+01,4.600558e+02,1.373593e+00,4.118559e+03,3.703426e-01)

scale_vec = c(7.736638e+02, 1.376919e+00,1.476503e-01,1.952408e-01,2.415097e+04,9.603810e+00,1.400209e+03,3.793172e+00,2.065135e+03,4.829568e-01)
#QUESTION 1.A

set.seed(425)
clustering_model = hclust(dist(data),method = 'complete')


silh=c()
for (k in 2:10){
  kume=cutree(clustering_model,k=k)
  X_sil=silhouette(kume, dist(data))
  silh[k-1]=mean(X_sil[,3])
}
data.frame(k=2:10,silh) #2 clusters seem appropriate for this situation


#QUESTION 1.B


set.seed(425)

clusters = cutree(clustering_model,k=2)
clusters



data_new = cbind(data,clusters)
data_new = data.frame(data_new)
centroid_1 = c()
centroid_2 = c()
for(i in 1:10){
  
  centroid_1[i] = mean(data_new[c(which(data_new$cluster == 1)),i])
  centroid_2[i] = mean(data_new[c(which(data_new$cluster == 2)),i])


}
centroid_1 
#-2.625680e-04 -3.203067e-04  9.835254e-05  6.283728e-05 -2.010275e-03 -6.010141e-03 -1.394388e-02 -1.263753e-02  9.300654e-04 -1.305387e-03

centroid_1_non_scale = centroid_1*scale_vec + center
centroid_1_non_scale
# 1.439114e+02 2.059074e+00 1.014519e+00 1.012265e+00 1.709630e+04 1.154418e+01 4.405315e+02 1.325657e+00 4.120480e+03 3.697122e-01



centroid_2
#0.26223984  0.31990627 -0.09822960 -0.06275873  2.00776234  6.00262786 13.92645087 12.62173226 -0.92890286  1.30375512

centroid_2_non_scale = centroid_2*scale_vec + center
centroid_2_non_scale

#3.470000e+02 2.500000e+00 1.000000e+00 9.999999e-01 6.563426e+04 6.925000e+01 1.996000e+04 4.924999e+01 2.200249e+03 1.000000e+00


#Cluster 1 is cluster that contains passengers with last_award = 0
#Cluster_2 is cluster that contains passengers with last_award = 1


#QUESTION 1.C


random_vector = sample(1:3999,200,replace = FALSE)

data_sub = data[-random_vector,]
data_sub

set.seed(425)
clustering_model_sub = hclust(dist(data_sub),method = 'complete')


silh_sub=c()
for (k in 2:10){
  kume_sub=cutree(clustering_model_sub,k=k)
  X_sil_sub=silhouette(kume_sub, dist(data_sub))
  silh_sub[k-1]=mean(X_sil_sub[,3])
}
data.frame(k=2:10,silh_sub) #2 clusters seem appropriate for this situation


set.seed(425)

clusters_sub = cutree(clustering_model_sub,k=2)
clusters_sub



data_new_sub = cbind(data_sub,clusters_sub)
data_new_sub = data.frame(data_new_sub)
centroid_1_sub = c()
centroid_2_sub = c()
for(i in 1:10){
  
  centroid_1_sub[i] = mean(data_new_sub[c(which(data_new_sub$cluster == 1)),i])
  centroid_2_sub[i] = mean(data_new_sub[c(which(data_new_sub$cluster == 2)),i])
  
  
}

centroid_1_sub
#0.0001550938  0.0036630694 -0.0018582893 -0.0020249651 -0.0017116571 -0.0076608895 -0.0158114197 -0.0170743607  0.0019382105  0.0013912364

centroid_2_sub
#0.26223984  0.31990627 -0.09822960 -0.06275873  2.00776234  6.00262786 13.92645087 12.62173226 -0.92890286  1.30375512



#NUMBER OF CLUSTERS DIDN'T CHANGE BUT CENTROIDS OF THE CLUSTER1 CHANGED WHERE CENTROIDS OF CLUSTER2 ARE SAME




#QUESTION 1.D

silh_kmean = c()

for(i in 2:20){
  kmeans_clus = kmeans(data,i,nstart = 20)
  sil = silhouette(kmeans_clus$cluster,dist(data))  
  silh_kmean[i-1] = mean(sil[,3])
}

data.frame(2:20,silh_kmean)  # 4 clusters look like the best option for number of clusters



#QUESTION 1.E
set.seed(425)

k_cluster = kmeans(data,4,nstart = 20)

sil = silhouette(k_cluster$cluster,dist(data))
plot(sil) 

set.seed(425)

data =cbind(data,'cluster' = k_cluster$cluster)
data

data = data.frame(data)
data[which(data$cluster == 3),] # I would target cluster 3 because its silhouette index is larger compared to others

centroid_3 = c()
for(i in 1:10){
  centroid_3[i] = mean(data[which(data$cluster == 3),i])
}
centroid_3

centroid_3_non_scale = centroid_3*scale_vec +center
centroid_3_non_scale

#100.3499323    1.2384135    1.0000004    1.0003927 4799.5081761    6.8900233  206.1328244    0.6358991 3712.6161733    0.2022781


#It seems these members of cluster are old customers who do not do transaction too much. We can offer them some promotions like discounts, free tickets 
#or miles in order to increase their retention and increase their commitment to the company. 
