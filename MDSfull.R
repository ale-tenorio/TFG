library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

#######################################################################
####################  DISTANCIAS CARRETERA ############################
#######################################################################
df.road <- read.csv(file = "D:/QGis/Capas nuevas/DATOS REALES/distancias reales.csv",header = TRUE,sep = ";")

lista_borrar <- df.road %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(A = as.character(origin_id),
                B = as.character(destination_id),
                d = total_cost) %>% 
  dplyr::select(A,B,d) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(is.na(d)) %>% 
  pull(A)

Estaciones <- read.csv(file = "D:/QGis/Capas nuevas/DATOS REALES/Estaciones.csv",header = TRUE,sep = ",") %>% 
  filter(!(ID %in% lista_borrar)) %>% 
  as_tibble()

road.mat <- df.road %>% 
  dplyr::mutate(A = as.character(origin_id),
                B = as.character(destination_id),
                d = total_cost) %>% 
  dplyr::select(A,B,d) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(! (A %in% lista_borrar | B %in% lista_borrar)) %>% 
  pivot_wider(names_from = B, values_from = d) %>% 
  column_to_rownames(var="A") %>% 
  as.dist()

road.sum <- road.mat %>% rowSums() %>%
  as.tibble() %>% dplyr::mutate(cuencas.road$NUMERO) %>% 
  dplyr::mutate(ID = `cuencas.road$NUMERO`,
                Sum = value) %>% 
  dplyr::select(ID, Sum)

MDS <-  df.road %>% 
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
  smacofSym(ndim = 3, type = "ordinal")

fit <-  

library(smacof)

df1 <- MDS %>% 
  .$conf %>% 
  as_tibble()


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
  merge(cuencas, by.x = 'A', by.y='NUMERO')

road <- bind_cols(df1, Estaciones)


# Copy coordinates from MDS into a new matrix
df1.Coords = df1$points #take coords from cmdscale solution

#Compute a new set of distances from the MDS coordinates delivered by cmdscale
df1.Dists <- dist(df1.Coords, diag=TRUE, upper=TRUE)

#String the two distance matrices out into two columns
#and calculate the correlation between these two columns
r <- cor(c(df.disim), c(df1.Dists))

#compute and display r squared from this correlation, this is variance accounted for
rsquared <- r * r  #compute
rsquared   #display

#compute F test on this correlation

#degrees of freedom for numerator = 1 
#compute degrees of freedom for denominator = N - 2 and display
freedom = NROW(c(df1.Dists)) - 2
freedom #display df for denominator

#compute and display F
Fvalue <- rsquared /((1 - rsquared)/freedom) #compute
Fvalue #display

#use pf to determine the p-value of this F at df = 1, freedom
pf(Fvalue, 1, freedom, lower.tail=FALSE)


#Plot por cuenca con nombre
plot.road.lab <- ggplot(road)+
  geom_text(aes(V1, V2, col=factor(cuenca_A), label=A))+
  theme_bw() + 
  guides(fill = FALSE)

#Plot por cuenca con color
#plot.road.col <- 
  ggplot()+
  geom_point(road %>% filter(!NO_VERTIEN %in% c("")),
             mapping = aes(D1, D2,col=factor(NO_VERTIEN)), size=2, alpha = 1)+ #, col=factor(NU_CUENCA)
 #   geom_point(road %>% filter(NO_VERTIEN %in% c("CARIBE", "PAC SUR", "PAC NORTE")),
  #           mapping = aes(V1, V2,col=factor(NO_VERTIEN)), size=2)+ #, col=factor(NU_CUENCA)
  theme_bw() + 
  labs(color = "VERTIENTE")+
  theme(legend.position = "none")


sdf <- asd$conf %>% as.tibble()

ggplot(sdf)+
  geom_point(aes(D1, D2))+
  theme_bw() 
  
  
library(plotly)

pl <- plot_ly(road, x = ~V1, y = ~V2, z = ~V3, color = ~V4, colors = 'Set1')

  





