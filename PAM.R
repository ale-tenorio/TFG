library(cluster)
library(ggpubr)
library(ggplot2)
source("Disimilaridades.R")

# Aplicacion de PAM para matrices de distancia
# Estimación del número óptimo de clusters por método de silueta
silhouette_pam <- function(dist, kmax = 10) {
sil = vector(length = kmax-1)
for(i in 2:kmax) {
  pam_res <- cluster::pam(x = dist, k = i)
  sil[i-1] <- cluster::silhouette(x = pam_res$clustering,dist = dist) %>% 
            summary() %>% 
            .$avg.width
}
plot_sil <- data.frame(k = 2:kmax,
                       avg.sil = sil)

ggplot(plot_sil, mapping = aes(factor(k),avg.sil, group = 1)) + 
  geom_point(color = "steelblue") + geom_line(color = "steelblue") + theme_classic() + 
  theme(text = element_text(size=12),axis.text = element_text(size = 12)) +
  labs(title = "Método de la silueta",
    x = "Número de clusters k",
    y = "Ancho promedio de silueta") + 
  geom_vline(xintercept = which.max(plot_sil$avg.sil), linetype = 2, 
             color = "steelblue")
}

silhouette_pam(road_mat) +
       labs(subtitle = "Distancias en carretera")

b <- silhouette_pam(time_mat) +
     labs(subtitle = "TV-1",
       y = "")
c <- silhouette_pam(time_mat2) +
       labs(subtitle = "TV-2",
       y = "")
d <- silhouette_pam(time_mat3) +
       labs(subtitle = "TV-3",
       y = "")

#ggpubr::ggarrange(a,b,c,d,ncol = 4)

res_pam <- cluster::pam(road_mat, k = 3)
