library(devtools)
library(ggplot2)
library(factoextra)
library(cluster)
library(NbClust)
library(fpc)
#devtools::install_github("kassambara/factoextra")

protein <- read.csv("../data/protein.csv")
rownames(protein) = protein$Country
protein$Country = NULL
protein.scaled = as.data.frame(scale(protein))

km <- kmeans(protein.scaled, 5)
km

aggregate(protein.scaled, by=list(cluster = km$cluster), mean)

fviz_cluster(km, data=protein.scaled)

nb <- NbClust(protein.scaled, distance = "euclidean",
              min.nc=2, max.nc=12,
              method="ward.D2", index="all")
fviz_nbclust(nb)+theme_minimal()

km.res <- kmeans(protein.scaled, 3)

