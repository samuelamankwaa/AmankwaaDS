setwd("C:/NEW ERA 2020/data_science/Home Work")

if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(FactoMineR, factoextra, tidyverse,rio)

#################################################################################data pca, mean, median
df<-import("cluster_df.csv")  
head(df)

df <-df %>% remove_rownames() %>% column_to_rownames(var = 'Country')
head(df)

df <- scale(df)
df <- data.frame(df)
head(df)

#################################################################################pca
# test <- data.frame(df[,-8])
# head(test)
df.pca <- PCA(df, graph = FALSE)
#################################################################################plot1 -scree
fviz_screeplot(df.pca, addlabels = TRUE, ylim = c(0, 50)) # scree plot1

#################################################################################plot2 -Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method")

#################################################################################plot3 -Silhouette method=2 clusters
fviz_nbclust(df, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

################################################################ Visualize
# df2 <- df %>% select(-GEOID,-cluster)
# eclust(df2,"kmeans",3,nstart=25) #model and viz

#################################################################################cluster
model <- kmeans(df, centers = 3, nstart=25)
model

#3 clusters: cluster 1 =8 memebers, cluster 2= 2 members, cluster 3= 16 members
# analysis was able to partition clusters with 47.5% accuracy

df$cluster <- model$cluster #add cluster to df
head(df)
str(head)

df$cluster <- as.factor(df$cluster)
fviz_pca_ind(df.pca,
             #label = "none", # hide individual labels
             habillage = df$cluster, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)
