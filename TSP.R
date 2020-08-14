library(TSP)
library(dplyr)
library(sp)
library(raster)
library(matrixStats)

num_rutas <- new %>% group_by(CLUST) %>% count %>% nrow()
rutas <- new
tz <- raster::getData('GADM', country = "CR", level = 1)
tz.prj <- sp::spTransform(tz, CRS("+proj=longlat +datum=WGS84 +no_defs")) 
#####
methods <- c("nn", "repetitive_nn", "nearest_insertion", "farthest_insertion", "cheapest_insertion", "arbitrary_insertion")
routes <- c(1:num_rutas)
tsp_analysis <- matrix(, nrow = length(methods), ncol = length(routes))
rownames(tsp_analysis) <- methods
colnames(tsp_analysis) <- routes

for (i in 1:1001) {
  for (metodo in 1:length(methods)) {
    for (ruta in 1:num_rutas) {
    
      lista_rutas <- new %>% 
                    dplyr::select(ID,CLUST) %>% 
                    filter(CLUST == ruta) %>%  
                    pull(ID) %>% as.vector()
      
      mat_rutas <- road %>% filter(ID %in% lista_rutas) %>% 
                    dplyr::select(D1,D2) %>% dist() %>% 
                    as.matrix(labels = TRUE) %>% 
                    cbind(lista_rutas) %>% 
                    as_tibble() %>% column_to_rownames(var = "lista_rutas") %>% 
                    as.dist()
      
      tsp <- as.TSP(mat_rutas)
      tsp <- insert_dummy(tsp, label = "cut")
      tour <- solve_TSP(tsp, method = methods[metodo])
      tsp_analysis[metodo,ruta] <- tour_length(tour)
    }
  }
tsp_sum <- tsp_analysis %>% rowSums()
tsp_total <- tsp_total %>% rbind(tsp_sum) %>% as.tibble()
print(i)
}

tsp_summary <- data.frame(PROM = colMeans(tsp_total),
                          STD = colSds(tsp_total %>% as.matrix())) %>% 
               rownames_to_column(var="METODO") 


ggplot(tsp_summary, mapping= aes(METODO,PROM))+
    geom_bar(stat = "identity", width = 0.5, color = "black", fill = "white") + 
    geom_errorbar(mapping = aes(ymin=PROM-STD ,ymax =PROM+STD), width = .4) +
    geom_text(mapping = aes(label=round(PROM,digits = 2)), hjust=2, color="black") +  
    theme_classic(base_size = 14) +
    coord_flip() + 
    labs(title="Aplicaciones de TSP",
         x = "",
         y = "Longitud de ruta")
  

for (ruta in 1:num_rutas) {
    
    lista_rutas <- new %>% 
      dplyr::select(ID,CLUST) %>% 
      filter(CLUST == ruta) %>%  
      pull(ID) %>% as.vector()
    
    mat_rutas <- road %>% filter(ID %in% lista_rutas) %>% 
      dplyr::select(D1,D2) %>% dist() %>% 
      as.matrix(labels = TRUE) %>% 
      cbind(lista_rutas) %>% 
      as.tibble() %>% column_to_rownames(var = "lista_rutas") %>% 
      as.dist()
    
    tsp <- as.TSP(mat_rutas)
    tsp <- insert_dummy(tsp, label = "cut")
    tour <- solve_TSP(tsp, method = "")   #SELECCIONAR UN UNICO METODO
    path <- cut_tour(tour, "cut") %>% as.data.frame() %>% rownames_to_column() %>% 
      dplyr::select(rowname) %>% 
      mutate(rank = c(1:length(lista_rutas)),
             ruta = rowname) %>%
      dplyr::select(ruta,rank) %>% as.tibble()
    
    rutas <- rutas %>% left_join(path, by = c("ID" = "ruta")) %>% as.tibble()

}
#####
orden_rutas <- new %>% dplyr::select(contains("rank")) 
orden_rutas[is.na(orden_rutas)] = 0
RANK <- orden_rutas %>% rowSums()

rutas <- rutas %>% 
  dplyr::mutate(RANK) %>% 
  dplyr::select(ID, NU_CUENCA, LON, LAT, NO_VERTIEN, CLUST, RANK)
#####

ggplot() + 
  geom_polygon(data = tz.prj, aes(long, lat, group = group), 
               fill = "#f1f4c7", color = "#afb38a") +
  xlim(-86, -82.5) + ylim(8, 11.25) + 
  xlab("Longitud") + ylab("Latitud") + 
  coord_equal() + 
  geom_point(rutas, mapping = aes(LON, LAT, col = CLUST)) + 
  #geom_point(new %>% filter(rank == max(rank)), mapping = aes(Longitud, Latitud), col = "red") + 
  geom_path(rutas[order(rutas$RANK),],
            mapping = aes(x = LON, y = LAT, group = CLUST, col = CLUST), size = 1) +
  labs(color = "Ruta") +
  theme_bw()






    
    