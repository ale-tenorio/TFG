library(Rtsne)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
#######################################################################
####################  DISTANCIAS CARRETERA ############################
#######################################################################
df <- read.csv(file = "D:/QGis/Capas nuevas/DATOS REALES/distancias reales.csv",header = TRUE,sep = ";")

lista_borrar <- df %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(A = as.character(origin_id),
                B = as.character(destination_id),
                d = total_cost) %>% 
  dplyr::select(A,B,d) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(is.na(d)) %>% 
  pull(A)

cuencas <- read.csv(file = "D:/QGis/Capas nuevas/DATOS REALES/cuencas reales.csv",header = TRUE,sep = ",") %>% 
  filter(!(NUMERO %in% lista_borrar)) %>% 
  as.tibble()

df1 <-  df %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(A = as.character(origin_id),
                B = as.character(destination_id),
                d = total_cost) %>% 
  dplyr::select(A,B,d) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
    filter(! (A %in% lista_borrar | B %in% lista_borrar)) %>% 
  pivot_wider(names_from = B, values_from = d) %>% 
  column_to_rownames(var="A") %>% 
  as.matrix() %>% 
  as.dist() %>% 
  Rtsne(dims = 3, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = TRUE, theta = 0.0) 
#%>% 
#  .$Y %>% as.tibble()

dec <- function(value) {
  conv <- round(value*1e6)/1e6
  return(conv)
}

min <- df1$itercosts %>% min() %>% dec()
max <- df1$itercosts %>% max() %>% dec()
it = ifelse(dec(df1$itercosts) == min,1,0)
it = (length(df1$itercosts) - sum(it) + 1) *50
it2 = it/50

tsne_analysis <- data.frame(data.frame(max,min,it,it2))


for (j in 1:999) {

  df1 <-  df %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(A = as.character(origin_id),
                  B = as.character(destination_id),
                  d = total_cost) %>% 
    dplyr::select(A,B,d) %>% 
    dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
    filter(! (A %in% lista_borrar | B %in% lista_borrar)) %>% 
    pivot_wider(names_from = B, values_from = d) %>% 
    column_to_rownames(var="A") %>% 
    as.matrix() %>% 
    as.dist() %>% 
    Rtsne(dims = 3, perplexity=50, verbose=TRUE, max_iter = 2000, is_distance = TRUE, theta = 0.0)   
min <- df1$itercosts %>% min() %>% dec()
max <- df1$itercosts %>% max() %>% dec()
it = ifelse(dec(df1$itercosts) == min,1,0)
it = (length(df1$itercosts) - sum(it) + 1) *50
it2 = it/50
tsne_analysis <- rbind(tsne_analysis,list(max,min,it,it2))
}

ggplot(tsne_analysis) + 
  geom_density(mapping = aes(min)) + 
  geom_vline(aes(xintercept=mean(min)),
             color="blue", linetype="dashed", size=1) + 
  theme_bw()




df1.knn <-  df %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(A = as.character(origin_id),
                B = as.character(destination_id),
                d = total_cost) %>% 
  dplyr::select(A,B,d) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(! (A %in% lista_borrar | B %in% lista_borrar)) %>% 
  pivot_wider(names_from = B, values_from = d) %>% 
  column_to_rownames(var="A") %>% 
  as.matrix() 

df2 <- df %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(A = as.character(origin_id),
                B = as.character(destination_id),
                d = total_cost) %>% 
  dplyr::select(A,B,d) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>%
  filter(! (A %in% lista_borrar | B %in% lista_borrar)) %>% 
  pivot_wider(names_from = B, values_from = d) %>% 
  dplyr::mutate(cuenca_A = str_extract(string = A, pattern = "^[:digit:]+") %>% as.numeric(),
                cuenca_A = ifelse(cuenca_A > 100, cuenca_A %/% 1000, cuenca_A)) %>% 
  dplyr::select(A,cuenca_A) %>% 
  merge(cuencas, by.x = 'A', by.y='Name')


road <- bind_cols(df1$Y %>% as.tibble(), cuencas)

#Plot por cuenca con nombre
plot.road.lab <- ggplot(road)+
  geom_text(aes(V1, V2, col=factor(cuenca_A), label=A))+
  theme_bw()

#Plot por cuenca con color
#plot.road.col <- 
ggplot()+
  geom_point(road %>% filter(!NO_VERTIEN %in% c("")),
             mapping = aes(V1, V2,col=factor(NO_VERTIEN)), size=2, alpha = 1)+ #, col=factor(NU_CUENCA)
  #   geom_point(road %>% filter(NO_VERTIEN %in% c("CARIBE", "PAC SUR", "PAC NORTE")),
  #           mapping = aes(V1, V2,col=factor(NO_VERTIEN)), size=2)+ #, col=factor(NU_CUENCA)
  #ggforce::geom_mark_hull(data = road %>% filter(V1 > 5, V1 < 13, V2 > -3),
  #                        mapping = aes(V1,V2), alpha = 0.1, fill = "grey50") +
  theme_bw() + 
  labs(color = "VERTIENTE",
       subtitle = "Perplejidad = 80")


  #rotheme(legend.position = "none")

  library(plotly)
  
  pl <- plot_ly(road, x = ~V1, y = ~V2, z = ~V3, color = ~NO_CUENCA, colors = 'Set1',name = df2$NU_CUENCA)
  
## Executing the algorithm on curated data
#tsne <- Rtsne(mat, dims = 2, perplexity=40, verbose=TRUE, max_iter = 5000, is_distance = TRUE, theta = 0.5)
#exeTimeTsne<- system.time(Rtsne(mat, dims = 2, perplexity=30, verbose=TRUE, max_iter = 5000, theta = 0.5))

