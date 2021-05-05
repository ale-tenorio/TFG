library(dplyr)
library(tidyr)
library(tidyverse)
library(smacof)
source("Disimilaridades.R")

disim_matrix = time_mat3
labels = Estaciones_2

# Metodo del codo para estimar numero optimo de dimensiones
MDSdim <- function(matrix, dmax = 10) {
matriz <- matrix(ncol = 2,nrow = dmax)
colnames(matriz) <- c("dim","stress")
  for (d in 1:dmax) {
    matriz[d,1] <- d
    matriz[d,2] <- smacof::smacofSym(matrix,ndim = d, type = "ordinal")$stress
  }
diff <- diff(matriz[,2])
plot <- ggplot(matriz %>% as.data.frame() %>% filter(!dim == 1), mapping = aes(factor(dim),stress, group = 1)) + 
  geom_point(color = "black") + geom_line(color = "black") + theme_classic() + 
  theme(text = element_text(size=16),axis.text = element_text(size = 12)) +
  xlab("Dimensión (p)") + 
  ylab("Porcentaje de stress") 

return(plot)
}

MDSdim(disim_matrix) +
  geom_vline(xintercept = 2, linetype = 2, 
             color = "red")

# Escalamiento no metrico aplicado sobre datos clase dist (3 DIMENSIONES)
MDS <-  disim_matrix %>% 
  smacof::smacofSym(ndim = 3, type = "ordinal")

df1 <- MDS %>% 
  .$conf %>% 
  as_tibble()

colnames(df1) <- paste("V", 1:(dim(df1)[2]), sep = "")

#Existe "Estaciones" (road), "Estaciones_2" (time) y "Estaciones_3" (euc)
res_mdsnm <- bind_cols(df1, labels)

#Gráfico de representación en 2 dimensiones de matriz original
ggplot()+
  geom_point(res_mdsnm %>% filter(!NO_VERTIEN %in% c("")),
             mapping = aes(V1, V2,col=factor(NO_VERTIEN)), size=2, alpha = 1)+ #, col=factor(NU_CUENCA)
  #   geom_point(road %>% filter(NO_VERTIEN %in% c("CARIBE", "PAC SUR", "PAC NORTE")),
  #           mapping = aes(V1, V2,col=factor(NO_VERTIEN)), size=2)+ #, col=factor(NU_CUENCA)
  ggrepel::geom_text_repel(res_mdsnm %>% filter(ID %in% c("98090","98050","79022","79014","14-12","69524")), 
                           mapping = aes(V1,V2, label = ID),
                           size = 5,
                           box.padding = 1.2,
                           segment.color = "black") +
  theme_bw() + 
  labs(color = "VERTIENTE") +
  theme(legend.position = "none")

  
  library(plotly)
  
  plot_ly(res_mdsnm, x = ~V1, y = ~V2, z = ~V3, color = ~NO_VERTIEN, type = "scatter3d", 
          colors = 'Set1', marker = list(size = 3)) %>% 
    layout(legend=list(title=list(text='<b> Vertiente </b>'))) %>% 
    layout(legend = list(orientation = 'h')) %>% 
    layout(legend = list( font = list(family = "sans-serif",size = 12, color = "#000"),
                          bgcolor = "white",
                          bordercolor = "black",
                          borderwidth = 2))

  
library(ggrepel)

# Se preparan los datos
stress <- MDS$spp %>% as.data.frame() %>% rownames_to_column("Estacion") 

stress <- stress[order(stress$.,decreasing = TRUE),] %>% 
  mutate(Orden = c(1:nrow(stress))) %>% as_tibble()

min.road <-MDS$delta %>% as.matrix()
min.road[min.road == 0] <- 1e7
min.road <- min.road %>%  apply(1, FUN = min) %>% as.data.frame() %>%  rownames_to_column() %>% as.tibble()
min.road <- min.road[order(min.road$.,decreasing = TRUE),]

# DISTRIBUCION PORCENTUAL DE STRESS
ggplot() + 
  geom_point(stress, mapping = aes(x=Orden,y=.))+
  geom_point(stress%>% filter(Estacion %in% min.road$rowname[1:5]), mapping = aes(x=Orden,y=.), color = "red")+
  ggrepel::geom_text_repel(stress %>% filter(. > 1), #Estacion %in% min.road$rowname[1:5]
            mapping = aes(x=Orden,y=., label = paste("Est. ",Estacion," | ",round(.,digits = 2),"%",sep = "")),
            size = 5,
            box.padding = 2,
            segment.color = "red",
            nudge_x = c(50,50,125,50,-50)) + 
  theme_bw()+
  theme(text = element_text(size=14)) + 
  labs(title="Descomposici?n porcentual de Stress",
       y = "Stress (%)",
       x = "Estaciones")

# Se preparan los datos
mds.data <- data.frame(Disimilaridades = MDS$delta %>% as.vector(),
                        Disparidades = MDS$dhat %>% as.vector(),
                        Distancias = MDS$confdist %>% as.vector()) %>% 
                        mutate(Residuos = Distancias - Disparidades,
                               StdRes = sqrt(abs(Residuos)),
                               DistanciasR = round(Distancias, digits = 1)) %>% 
                        as.tibble()

# DIAGRAMA DE SHEPARD
ggplot(data = mds.data) +
  geom_point(mapping = aes(Disimilaridades, Distancias), color = "gray") +
  geom_point(mapping = aes(Disimilaridades, Disparidades)) + 
  theme_bw() + 
  theme(text = element_text(size=14), 
        panel.grid.minor = element_blank()) + #panel.grid.major = element_blank(), 
  labs(title = "Diagrama de Shepard",
       y = "Distancias y Disparidades") 


# DIAGRAMA DE RESIDUOS 
mds.res1 <- ggplot(data = mds.data) +
              geom_point(mapping = aes(Distancias, Residuos), shape = 1, color = "black") +
              geom_hline(yintercept = 0, linetype = "dashed", col = "gray") + 
              #geom_line(mapping = aes(Distancias, Residuos.y), color = "red") +
              geom_smooth(mapping = aes(Distancias, Residuos), method = "lm", formula = y ~ poly(x, 2), color = "red") +  
              theme_bw() + 
              theme(text = element_text(size=14),
                    panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
              ylim(-0.35,0.35) +
              labs(title = "Gráfico de residuos",
                   x = "Distancias",
                   y = "Residuos")

# DIAGRAMA DE RESIDUOS NORMALIZADOS
mds.res2 <- ggplot(data = mds.data) +
              geom_point(mapping = aes(Distancias, StdRes), shape = 1, color = "black") +
              geom_smooth(mapping = aes(Distancias, StdRes), method = "lm",formula = y ~ poly(x, 2), color = "red") +
              theme_bw() + 
              theme(text = element_text(size=14),
                    panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
              # geom_hline(yintercept = 0, linetype = "dashed") + 
              #  ylim(-0.35,0.35) +
              labs(title = "Gráfico de escala-ubicación",
                   x = "Distancias",
                   y = expression(sqrt("Residuos estandarizados"))) 

ggpubr::ggarrange(mds.res1,mds.res2,ncol = 2)
