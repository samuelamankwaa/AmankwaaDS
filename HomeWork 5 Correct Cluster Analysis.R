setwd("C:/NEW ERA 2020/data_science/Home Work")

if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(FactoMineR, factoextra, tidyverse,rio)

#################################################################################data pca, mean, median
df5<-import("cluster_pca.csv")  
head(df5)
str(df5)

#df5$GEOID <- as.character(df5$GEOID)
# df5$GEOID <- as.numeric(df5$GEOID)
# df5$GEOID <- as.factor(df5$GEOID)

df5 <-df5 %>% remove_rownames()%>% column_to_rownames(var = 'GEOID')  
#rownames(df5) <- c()
#rownames(df5) <- NULL
#df5 <- print(df5, row.names = FALSE)
head(df5)

df5 <- scale(df5)
df5 <- data.frame(df5)
head(df5)

#################################################################################pca
# test <- data.frame(df[,-8])
# head(test)
df5.pca <- PCA(df5, graph = FALSE)
#################################################################################plot1 -scree
fviz_screeplot(df5.pca, addlabels = TRUE, ylim = c(0, 50)) # scree plot1

#################################################################################plot2 -Elbow method
fviz_nbclust(df5, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method")

#################################################################################plot3 -Silhouette method=2 clusters
fviz_nbclust(df5, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

################################################################ Visualize
# df2 <- df %>% select(-GEOID,-cluster)
# eclust(df2,"kmeans",3,nstart=25) #model and viz

#################################################################################cluster
model6 <- kmeans(df5, centers = 5, nstart=25)
model6

#3 clusters: cluster 1 =20 members, cluster 2= 16 members, cluster 3= 51 members, cluster 4= 66 members, cluster 5= 3 members
# analysis was able to partition clusters with 50.6% accuracy

## Clusters breakdown: cluster 1 =20 members, cluster 2= 16 members, cluster 3= 51 members, cluster 4= 66 members, cluster 5= 3 members

# K-means clustering with 4 clusters
#model6 <- kmeans(df5, centers = 4, nstart=25)
# View the model
#model6
## Clusters breakdown: cluster 1 =66 members, cluster 2= 20 members, cluster 3= 67 members, cluster 4= 3 members.


df5$cluster <- model6$cluster #add cluster to df
head(df5)
str(df5)

df5$cluster <- as.factor(df5$cluster)
str(df5)

fviz_pca_ind(df5.pca,
             #label = "none", # hide individual labels
             habillage = df5$cluster, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "#E69F00",'black'),
             addEllipses = TRUE # Concentration ellipses
)
