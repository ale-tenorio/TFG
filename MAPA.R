#read in libraries
library(ggmap)
library(sp)
library(raster)
library(ggplot2)
source("K-Means.R")
source("PAM.R")
# Opción de representar resultados de k-medias
map_data <- redDim %>% 
       mutate(CLUST = kmeans_plot$CLUST)
# Opción de representar resultados de k-medoides
map_data2 <- Estaciones %>% 
       mutate(CLUST = res_pam$clustering)
#
map_data <- optics_plot

  
#Leer el Shapefile de Costa Rica
tz <- raster::getData('GADM', country = "CR", level = 1)
tz.prj <- sp::spTransform(tz, CRS("+proj=longlat +datum=WGS84 +no_defs")) 

#Hullmap permite visualizar los clusters
hull <- map_data %>%
  group_by(CLUST) %>%
  dplyr::slice(chull(LON, LAT))
#MAPA K-MEDIAS
        ggplot() + 
                geom_polygon(data = tz.prj, aes(long, lat, group = group), 
                fill = "#f1f4c7", color = "#afb38a") +
                        xlim(-86, -82.5) + ylim(8, 11.25) + 
                        xlab("Longitud") + ylab("Latitud") +
                        aes(fill = factor(CLUST))+
                        geom_polygon(hull, mapping = aes(x=LON, y=LAT, colour = as.factor(CLUST)), alpha = 0.1) +
                        #geom_point(new %>% filter(!clust == 5), 
                        #           mapping = aes(x=Longitud, y=Latitud))+
                        geom_point(map_data %>% dplyr::filter(!CLUST %in% ""),
                                   mapping = aes(x=LON, y=LAT, col = factor(CLUST))) +
                        guides(fill = FALSE) + 
                        theme_bw() + 
                        coord_equal()+
                        theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                        size = 0.5), panel.background = element_rect(fill = "white"), 
                        panel.border = element_rect(fill = NA)) +
                        labs(color = "Cluster",
                             title = "")  +
                        theme(legend.position = "none",
                              axis.title = element_blank(), 
                              axis.text = element_blank())
        
hull <- map_data2 %>%
  group_by(CLUST) %>%
  dplyr::slice(chull(LON, LAT))
# MAPA K-MEDOIDES        
        ggplot() + 
                geom_polygon(data = tz.prj, aes(long, lat, group = group), 
                             fill = "#f1f4c7", color = "#afb38a") +
                             xlim(-86, -82.5) + ylim(8, 11.25) + 
                             xlab("Longitud") + ylab("Latitud") +
                             aes(fill = factor(CLUST))+
                geom_polygon(hull, mapping = aes(x=LON, y=LAT, colour = as.factor(CLUST)), alpha = 0.1) +
                #geom_point(new %>% filter(!clust == 5), 
                #           mapping = aes(x=Longitud, y=Latitud))+
                geom_point(map_data2 %>% dplyr::filter(!CLUST %in% ""),
                           mapping = aes(x=LON, y=LAT, col = factor(CLUST))) +
                guides(fill = FALSE) + 
                ggrepel::geom_label_repel(map_data2 %>% dplyr::filter(ID %in% res_pam$medoids),
                                          mapping = aes(LON,LAT, label=ID), fill = "white", hjust = 0, vjust = 0,
                                          min.segment.length = 0) +
                geom_point(map_data2 %>% dplyr::filter(ID %in% res_pam$medoids),
                           mapping = aes(LON,LAT), color = "black", shape = 18, size = 3) +
                theme_bw() + 
                coord_equal()+
                theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                                      size = 0.5), panel.background = element_rect(fill = "white"), 
                      panel.border = element_rect(fill = NA)) +
                labs(color = "Cluster",
                     title = "Tiempos de viaje (visitas ampliadas)")  +
                theme(legend.position = "none",
                      axis.title = element_blank(), 
                      axis.text = element_blank())
           

      
      #ggpubr::ggarrange(m2,m1,ncol = 2)

      #map_data %>% filter(CLUST == 3) %>% group_by(NO_VERTIEN) %>% count

