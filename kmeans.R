library(stats)
library(tidyverse)
library(cluster)
library(factoextra)
library(dplyr)
library(NbClust)

#mengimport data dari excel
data_kesmas <- read_excel("~/!Vivi Alfia/,TUGAS AKHIR/Data-data/data kesmas.xlsx")
View(data_kesmas)

#analisis deskriptif
d1 <- (data_kesmas[,3:8])
d1
summary(d1)

#standarisasi data
datafix <- scale(d1)       
datafix
head(datafix)
View(datafix)

#Uji Multikolinearitas
cor(datafix,datafix)

##Mencari Nilai K Optimal 
#Metode  ELBOW atau WSS
fviz_nbclust(datafix, kmeans, method = "wss")
#Metode SILHOUETTE
fviz_nbclust(datafix, kmeans, method = "silhouette")
#Metode GAP STATISTIC
dim(d1)
set.seed(9999)      #Mengunci data ,nilainya ditentukan sendiri
gap_stat <-clusGap(datafix, FUN=kmeans, nstart=25, K.max=10, B= 150)
fviz_gap_stat(gap_stat)

##Membuat Cluster K Means##
final <- kmeans(datafix, 3)
print(final)
final$centers
final$iter
fviz_cluster(final, data = datafix)

#Hasil pengelompokkan data
finalakhir=data.frame(final$cluster)
head(finalakhir)
View(finalakhir)

#Profiling dan karakterisasi
(data_kesmas[,3:8]) %>%       
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#Validasi cluster
shasil <- silhouette(final$cluster, dist(datafix))
summary(shasil)
plot(shasil, main = "Silhouette Coefficient of K-Means")

