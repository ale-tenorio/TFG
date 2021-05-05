library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
source("Disimilaridades.R")

disim_matrix = time_mat3
labels = Estaciones_2

#Escalamiento clásico aplicado sobre datos clase dist
MDSC <-  disim_matrix %>% 
  cmdscale(k = 3, eig = TRUE)

#Extracción de valores principales
eig <- data.frame(index = c(1:length(MDSC$eig)),
                  eig = MDSC$eig) %>% as_tibble()

#Criterios de elección de dimensiones
Criterios_MDSC <- function() {
  total_abs_eig <- sum(abs(eig$eig))
  i = 1
  while(sum(abs(eig$eig[1:i]))/total_abs_eig < 0.8) {
    i = i+1
  }
  
  total_2_eig <- sum((eig$eig)^2)
  j = 1
  while(sum((eig$eig[1:j])^2)/total_2_eig < 0.8) {
    j = j+1
  }
  a <- matrix(ncol = 2,nrow = 2,data = c(sum(abs(eig$eig[1:i]))/total_abs_eig,sum((eig$eig[1:j])^2)/total_2_eig,i,j),
              dimnames = list(c("Criterio Pm(1)","Criterio Pm(2)"),c("Pm > 0.8","Dimensiones")))  
  
  total_eig <- sum(eig$eig)
  cumsum <- cumsum(eig$eig)
  diff <- abs(cumsum-total_eig)
  i = 1
  while (diff[i+1] < diff[i]) {
    i = i+1
  }
  
  b <- matrix(ncol = 3,nrow = 1,data = c(total_eig,sum(eig$eig[1:i]),i),
              dimnames = list(c("Criterio de traza"),c("Sum eig","Resultado","Dimensiones")))  
  
  min_eig <- min(eig$eig)
  i = 0
  while (eig$eig[i+1] > abs(min_eig)) {
    i = i+1
  }  
  c <- matrix(ncol = 3,nrow = 1,data = c(min_eig,eig$eig[i],i),
              dimnames = list(c("Criterio de magnitud"),c("eig min","Resultado","Dimensiones admisibles"))) 
  
  return(list(a,b,c))
}
Criterios_MDSC()

#Se crea dataframe con etiquetas de los resultados
df1 <- MDSC %>% 
  .$points %>% 
  as_tibble()

#Existe "Estaciones" (road), "Estaciones_2" (time) y "Estaciones_3" (euc)
res_mdsc <- bind_cols(df1, labels) 



#Gráfico de valores principales
ggplot() +
    geom_point(eig %>% filter(eig > 0, index <= 10), mapping = aes(index,eig), color = "blue") +
  geom_bar(eig %>% filter(eig > 0, index <= 10), mapping = aes(index,eig), stat = "identity", color = "blue", width = 0.1) +
  #geom_point(eig %>% filter(eig < 0), mapping = aes(index,eig), color = "red") +
  #geom_bar(eig %>% filter(eig < 0), mapping = aes(index,eig), stat = "identity", color = "red", width = 0.1) +
  theme_bw() + 
  #ylim(-22500000000.,22500000000) +
  theme(text = element_text(size=14)) +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(subtitle = "",
       x = "",
       y = "")

labs(subtitle = "Gráfico de valores principales",
     x = "Dimensión",
     y = "")

#Gráfico de representación en 2 dimensiones de matriz original
ggplot()+
  geom_point(res_mdsc %>% filter(!NO_VERTIEN %in% c("")),
             mapping = aes(V1, V2,col=factor(NO_VERTIEN)), size=2, alpha = 1) + 
  ggrepel::geom_text_repel(res_mdsc %>% filter(ID %in% c("")), #98090","98050","79022","79014","14-12","69524
                           mapping = aes(V1,V2, label = ID),
                           size = 5,
                           box.padding = 1.2,
                           segment.color = "black") +
  theme_bw() + 
  labs(color = "VERTIENTE") +
  theme(legend.position = "none")

#Se prepara información para análisis de regresión simple
mdsc.data <- data.frame(Disimilaridades = disim_matrix %>% as.vector(),
                         Distancias = df1 %>% dist() %>% as.vector()) %>% 
             as_tibble()

fit <- mdsc.data %>% lm(Distancias~Disimilaridades, data =.)

par(mfrow = c(1,4))
plot(fit)

library(plotly)

plot_ly(res_mdsc, x = ~V1, y = ~V2, z = ~V3, color = ~NO_VERTIEN, name = ~ID, type = "scatter3d", 
        colors = 'Set1', marker = list(size = 3)) %>% 
  layout(legend=list(title=list(text='<b> Vertiente </b>'))) %>% 
  layout(legend = list(orientation = 'h')) %>% 
  layout(legend = list(font = list(family = "sans-serif",size = 12, color = "#000"),
                       bgcolor = "white",
                       bordercolor = "black",
                       borderwidth = 2))


