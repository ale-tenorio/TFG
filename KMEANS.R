library(factoextra)
library(dplyr)
library(ggpubr)
library(ggplot2)

#dataframe = road #MATRIZ DE COORDENADAS
dataframe = road

data = dplyr::select(dataframe, D1, D2,D3) #ELEGIR EL NUMERO DE DIMENSIONES
#data = dplyr::select(dataframe, samples.1,samples.2)
name = dplyr::select(dataframe, NOMBRE)
cuenca = dplyr::select(dataframe, NU_CUENCA)


# Number of clusters


# Elbow method
fviz_nbclust(data, kmeans, method = "wss", print.summary = TRUE) +
  geom_vline(xintercept = 6, linetype = 2)+
 # geom_vline(xintercept = 5, linetype = 3)+
  labs(subtitle = "Método del codo",
       title = "Número óptimo de clusters",
       x = "Número de clusters k",
       y = "Suma de WSS")


# Sillouette method
fviz_nbclust(data, kmeans, method = "silhouette",)+
  geom_vline(xintercept = 6, linetype = 3)+
  labs(subtitle = "Método de la silueta",
       title = "Número óptimo de clusters",
       x = "Número de clusters k",
       y = "Ancho promedio de la silueta")
# K-means clustering
clust <- kmeans(data, 6)$cluster %>%
  as.factor()
data <- data %>%
  mutate(CLUST = clust,
         VERTIENTE = cuencas$NO_VERTIEN,
         NO_CUENCA = cuencas$NO_CUENCA,
         NU_CUENCA = cuencas$NU_CUENCA)



hull <- data %>%
  group_by(groups) %>%
  slice(chull(D1,D2))


ggplot() +
  geom_point(data, mapping = aes(x=D1, y=D2,colour = factor(groups)))+
  aes(fill = factor(groups))+
  geom_polygon(hull, mapping = aes(x=D1,y=D2,colour = groups),alpha = 0.1) +
  theme_bw() +
  guides(fill = FALSE) +
  labs(color = "Cluster")+
  xlab("V1") + ylab("V2")

# Plot and color by groups
plot.kmeans.labs <- ggscatter(data, x = "V1", y = "V2", 
                              #label = "lab",
                              font.label = c(8),
                              color = "groups",
                              palette = "jco",
                              size = 1, 
                              ellipse = TRUE,
                              ellipse.type = "convex",
                              xlab = "V1",
                              ylab = "V2",
                              repel = FALSE) + theme_bw()


grupos <- data %>% dplyr::select(lab,groups, cuenca)

pl.3d <- plot_ly(road, x = ~V1, y = ~V2, z = ~ V3, color = grupos$groups)







