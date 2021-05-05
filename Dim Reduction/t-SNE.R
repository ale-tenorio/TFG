library(Rtsne)
library(dplyr)
library(tidyr)
library(tidyverse)
source("Disimilaridades.R")

disim_matrix = time_mat3
labels = Estaciones_2

# Busca la inicialización aleatoria con menor convergencia KL
best_tsne <- function(reps,perplexity,max_iter) {
    dec <- function(value) {
      conv <- round(value*1e6)/1e6
      return(conv)
    }
    tsne_analysis <- matrix(nrow = reps, ncol = 4) %>% as.tibble()
    colnames(tsne_analysis) <- c("seed","max","min","iter")
        for (j in 1:reps) {
        set.seed(j)
          TSNE <-  disim_matrix %>% 
          Rtsne(dims = 3, perplexity=perplexity, verbose=TRUE, max_iter = max_iter, is_distance = TRUE, theta = 0.0) 
        tsne_analysis$seed[j] = j
        tsne_analysis$min[j] = TSNE$itercosts %>% min() %>% dec()
        tsne_analysis$max[j] = TSNE$itercosts %>% max() %>% dec()
        conv = ifelse((tsne_analysis$min[j] == TSNE$itercost %>% dec()),0,1)
        tsne_analysis$iter[j] = (sum(conv)+1)*50
        }
    tsne_analysis[order(tsne_analysis$min),]
}

tsne_analysis <- best_tsne(reps = 1,perplexity = 50,max_iter = 3000)
head(tsne_analysis)

# Densidad de resultados obtenidos para mejor inicialización
ggplot(tsne_analysis) + 
  geom_density(mapping = aes(min)) + 
  geom_vline(aes(xintercept=mean(min)),
             color="blue", linetype="dashed", size=1) + 
  theme_bw()

#set.seed(823) #DISTANCE MATRIX
#set.seed(441) #CASO 1 TIME MATRIX
#set.seed(658) #CASO 2 TIME MATRIX
#set.seed(66) #CASO 3 TIME MATRIX

# Se hace elección de la semilla con menor valor KL
set.seed(66)
TSNE <-  disim_matrix %>% 
         Rtsne(dims = 3, perplexity=50, verbose=TRUE, max_iter = 5000, is_distance = TRUE, theta = 0.0) 

res_tsne <- bind_cols(TSNE$Y %>% as_tibble(), labels)


ggplot()+
  geom_point(res_tsne %>% filter(!NO_VERTIEN %in% c("")),
             mapping = aes(V1,V3), size=2, col="black", alpha = 1)+ 
  theme_minimal() + 
  theme(plot.subtitle = element_text(size = 24)) +
  labs(color = "VERTIENTE")  +
  theme(legend.position = "right") +
  labs(#title = "Caso 1. Estándar (TV-1)",
    #subtitle = "(TV-3)                      Perplejidad = 20")
    subtitle = "                                Perplejidad = 50")

ggplot()+
  geom_point(res_tsne %>% filter(!NO_VERTIEN %in% c("")),
             mapping = aes(V3,V2,col=factor(NO_VERTIEN)), size=2, alpha = 1)+ #, col=factor(NU_CUENCA)
  #   geom_point(road %>% filter(NO_VERTIEN %in% c("CARIBE", "PAC SUR", "PAC NORTE")),
  #           mapping = aes(V1, V2,col=factor(NO_VERTIEN)), size=2)+ #, col=factor(NU_CUENCA)
  #ggforce::geom_mark_hull(data = road %>% filter(V1 > 5, V1 < 13, V2 > -3),
  #                        mapping = aes(V1,V2), alpha = 0.1, fill = "grey50") +
  ggrepel::geom_text_repel(res_tsne %>% filter(ID %in% c("98090","98050","79022","79014","14-12","69524")), 
                           mapping = aes(V3,V2, label = ID),
                           size = 5,
                           box.padding = 1.2,
                           segment.color = "black") +
  theme_bw() + 
  labs(color = "VERTIENTE")  +
  theme(legend.position = "none")+
  labs(subtitle = "")


library(plotly)

m <- list(
  l = 50,
  r = 25,
  b = 0,
  t = 0,
  pad = 4
)

plot_ly(res_tsne, x = ~V1, y = ~V2, z = ~V3, color = ~NO_VERTIEN, type = "scatter3d", 
        colors = 'Set1', marker = list(size = 3)) %>% 
  layout(legend=list(title=list(text='<b> Vertiente </b>'))) %>% 
  layout(legend = list(orientation = 'h')) %>%
  layout(legend = list(x = -0.05)) %>% 
  layout(legend = list(font = list(family = "sans-serif",size = 12, color = "#000"),
    bgcolor = "white",
    bordercolor = "black",
    borderwidth = 2)) %>% 
    layout(autosize = F, width = 598, height = 601, margin = m)
  


