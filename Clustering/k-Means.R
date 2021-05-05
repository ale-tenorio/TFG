library(factoextra)
library(dplyr)
library(ggpubr)
library(ggplot2)
source("Calinski_Harabasz.R")
source("MDS clasico.R")
source("Disimilaridades.R")

redDim = res_tsne #euc
Est = Estaciones_2

data = dplyr::select(redDim, V1, V2, V3) #ELEGIR EL NUMERO DE DIMENSIONES

# Numero optimo de clusters

# Metodo del codo
elbow <- factoextra::fviz_nbclust(data, kmeans, method = "wss", print.summary = TRUE) +
# geom_vline(xintercept = 5, linetype = 3)+
  labs(subtitle = "Método del codo",
       title = "Número óptimo de clusters",
       x = "Número de clusters (k)",
       y = "Suma de WSS")

# Estadistico Gap
gap <- factoextra::fviz_nbclust(data, kmeans, method = "gap_stat") + 
  labs(subtitle = element_blank(),
       title = element_blank(),
       x = "Número de clusters k",
       y = "Estadístico Gap")

# Metodo de la silueta
silhouette <- factoextra::fviz_nbclust(data, kmeans, method = "silhouette")+
  labs(subtitle = "Método de la silueta",
       title = "",
       x = "Número de clusters (k)",
       y = "Ancho promedio de la silueta")

# Criterio Calinski-Harabasz
CH <- CHCriterion(data,kmax = 10,clustermethod = "kmeanspp")$data 

CH <- ggplot(CH, mapping = aes(factor(k),CHIndex, group = 1)) + 
  geom_point(color = "steelblue") + geom_line(color = "steelblue") + theme_classic() + 
  theme(text = element_text(size=12),axis.text = element_text(size = 12)) +
  labs(subtitle = "Criterio Calinski-Harabasz",
       title = "",
       x = "Número de clusters (k)",
       y = "Indice CH") + 
  geom_vline(xintercept = which.max(CH$CHIndex), linetype = 2, 
             color = "steelblue")


ggpubr::ggarrange(elbow,gap,silhouette,CH,ncol = 4)

# Funcion para obtener el resultado de k-medias de menor BSS/TSS
kmeans_optimal <- function(k, rep = 1000) {
kmeans_analysis <- matrix(nrow = rep, ncol = 5) %>% as_tibble()
colnames(kmeans_analysis) <- c("seed","withinss","betweenss","totss","BSS/TSS")
  for(i in 1:rep) {
    set.seed(i)
    clust <- kmeans(data, k)
    kmeans_analysis$seed[i] = i
    kmeans_analysis$withinss[i] = clust$tot.withinss
    kmeans_analysis$betweenss[i] = clust$betweenss
    kmeans_analysis$totss[i] = clust$totss
    kmeans_analysis$`BSS/TSS`[i] = (clust$betweenss/clust$totss)*100
  }
kmeans_optimal <- kmeans_analysis[order(kmeans_analysis$`BSS/TSS`, decreasing = TRUE),] 
best_seed <- kmeans_optimal %>% head(1) %>% pull(seed)
set.seed(best_seed)
kmeans_res <- kmeans(data, k)
output <- list("kmeans_optimal" = kmeans_optimal, "best_seed" = best_seed, "kmeans_res" = kmeans_res)
return(output)
}

kmedias <- kmeans_optimal(k = 4)

#Se prepara la informacion
kmeans_plot <- data %>%
  mutate(CLUST = kmedias$kmeans_res[["cluster"]],
         VERTIENTE = Est$NO_VERTIEN,
         NO_CUENCA = Est$NO_CUENCA,
         NU_CUENCA = Est$NU_CUENCA)

hull <- kmeans_plot %>%
  group_by(CLUST) %>%
  dplyr::slice(chull(V1,V2))

ggplot() +
  geom_point(kmeans_plot, mapping = aes(x=V1, y=V2, colour = as.factor(VERTIENTE)))+
  aes(fill = factor(CLUST))+
  geom_polygon(hull, mapping = aes(x=V1,y=V2),col = "dark gray",alpha = 0, size = 1) +
  theme_bw() +
  guides(fill = FALSE) +
  labs(color = "Cluster")+
  xlab("V1") + ylab("V2")+
  theme(legend.position = "none", 
        axis.title = element_blank(), 
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())








