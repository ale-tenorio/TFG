library(TSP)
library(dplyr)
library(sp)
library(raster)
library(matrixStats)
library(tibble)
library(ggplot2)
source("MAPA.R")

# Función para buscar la resolución de TSP con ruta más corta
tsp_selection <- function(rutas_data, pruebas = 1000, dist = 0) {
  tipo <- ifelse(data.class(dist) == "dist","disimilarity","redDim")
  if (stringr::str_detect(colnames(map_data),"^V") %>% head(1) == TRUE && tipo == "disimilarity") {
    stop("Quitar el parámetro -dist- si los datos provienen de reducción de dimensión")
  }
  if (stringr::str_detect(colnames(map_data),"^V") %>% head(1) == FALSE && tipo == "redDim") {
    stop("Falta añadir el parámetro -dist- con la matriz de disimiladidades correspondiente")
  }
  num_rutas <- rutas_data %>% dplyr::select(CLUST) %>% max()
  
  methods <- c("nn", "repetitive_nn", "nearest_insertion", "farthest_insertion",
               "cheapest_insertion", "arbitrary_insertion")
  
  dims <- stringr::str_which(colnames(rutas_data),"^V") %>% tail(1)
  routes <- c(1:num_rutas)
  tsp_analysis <- matrix(nrow = length(methods), ncol = length(routes))
  rownames(tsp_analysis) <- methods
  colnames(tsp_analysis) <- routes
  tsp_total <- 0
  a <- dist %>% as.matrix()

  for (i in 1:pruebas) {
    for (metodo in 1:length(methods)) {
      for (ruta in 1:num_rutas) {
        lista_rutas <- rutas_data %>% 
                      dplyr::select(ID,CLUST) %>% 
                      filter(CLUST == ruta) %>%  
                      pull(ID) %>% as.vector()
        if (tipo == "redDim") {
        mat_rutas <- rutas_data %>% filter(ID %in% lista_rutas) %>% 
                      dplyr::select(c(1:dims)) %>% dist() %>% 
                      as.matrix(labels = TRUE) %>% 
                      cbind(lista_rutas) %>% 
                      as_tibble() %>% tibble::column_to_rownames(var = "lista_rutas") %>% 
                      as.dist()
        } else {
        mat_rutas <- a[intersect(rownames(a),lista_rutas),intersect(rownames(a),lista_rutas)] %>% 
                     as.dist()
        }
        
        tsp <- as.TSP(mat_rutas)
        tsp <- insert_dummy(tsp, label = "cut")
        tour <- solve_TSP(tsp, method = methods[metodo])
        tsp_analysis[metodo,ruta] <- tour_length(tour)
      }
    }
  tsp_sum <- tsp_analysis %>% rowSums()
  tsp_total <- tsp_total %>% rbind(tsp_sum) %>% as_tibble()
  ifelse(i %% 10 == 0,print(paste("Rutas analizadas:",i)),"")
  }

tsp_summary <- data.frame(PROM = colMeans(tsp_total[-1,]),
                          STD = matrixStats::colSds(tsp_total[-1,] %>% as.matrix())) %>% 
               tibble::rownames_to_column(var="METODO") 


ggplot(tsp_summary, mapping= aes(METODO,PROM))+
    geom_bar(stat = "identity", width = 0.5, color = "black", fill = "white") + 
    geom_errorbar(mapping = aes(ymin=PROM-STD ,ymax =PROM+STD), width = .4) +
    geom_text(mapping = aes(label=round(PROM,digits = 2)), hjust=2, color="black") +  
    theme_classic(base_size = 14) +
    coord_flip() + 
    labs(title="Aplicaciones de TSP",
         x = "",
         y = "Longitud de ruta")
}

# Función que aplica el método encontrado y genera las rutas para los
# grupos encontrados anteriormente
tsp_create <- function(rutas_data,metodo,dist = 0) {
  tipo <- ifelse(data.class(dist) == "dist","disimilarity","redDim")
  if (stringr::str_detect(colnames(map_data),"^V") %>% head(1) == TRUE && tipo == "disimilarity") {
    stop("Quitar el parámetro -dist- si los datos provienen de reducción de dimensión")
  }
  if (stringr::str_detect(colnames(map_data),"^V") %>% head(1) == FALSE && tipo == "redDim") {
    stop("Falta añadir el parámetro -dist- con la matriz de disimiladidades correspondiente")
  }
  if (!metodo %in% c("nn", "repetitive_nn", "nearest_insertion", "farthest_insertion",
                     "cheapest_insertion", "arbitrary_insertion")) {
    stop("Método no valido. Usar nn, repetitive_nn, nearest_insertion, farthest_insertion, cheapest_insertion, arbitrary_insertion.")
  }
  num_rutas <- rutas_data %>% dplyr::select(CLUST) %>% max()
  dims <- stringr::str_which(colnames(rutas_data),"^V") %>% tail(1)
  a <- dist %>% as.matrix()
  rutas = rutas_data
  tsp_length <- matrix(ncol = num_rutas)
  colnames(tsp_length) <- c(1:num_rutas)
  rownames(tsp_length) <- "tour"
  for (ruta in 1:num_rutas) {
    
    lista_rutas <- rutas_data %>% 
      dplyr::select(ID,CLUST) %>% 
      filter(CLUST == ruta) %>%  
      pull(ID) %>% as.vector()
    
    if (tipo == "redDim") {
      mat_rutas <- rutas_data %>% filter(ID %in% lista_rutas) %>% 
        dplyr::select(c(1:dims)) %>% dist() %>% 
        as.matrix(labels = TRUE) %>% 
        cbind(lista_rutas) %>% 
        as_tibble() %>% tibble::column_to_rownames(var = "lista_rutas") %>% 
        as.dist()
    } else {
      mat_rutas <- a[intersect(rownames(a),lista_rutas),intersect(rownames(a),lista_rutas)] %>% 
        as.dist()
    }
    
    tsp <- as.TSP(mat_rutas)
    tsp <- insert_dummy(tsp, label = "cut")
    tour <- solve_TSP(tsp, method = metodo)   
    path <- cut_tour(tour, "cut") %>% as.data.frame() %>% tibble::rownames_to_column() %>% 
      dplyr::select(rowname) %>% 
      mutate(rank = c(1:length(lista_rutas)),
             ruta = rowname) %>%
      dplyr::select(ruta,rank) %>% as_tibble()
    tsp_length[1,ruta] <- attr(tour, "tour_length")
    rutas <- rutas %>% left_join(path, by = c("ID" = "ruta")) %>% as_tibble()
    
  }
  orden_rutas <- rutas %>% dplyr::select(contains("rank")) 
  orden_rutas[is.na(orden_rutas)] = 0
  RANK <- orden_rutas %>% rowSums()
  
  rutas <- rutas_data %>% 
    dplyr::mutate(RANK) %>% 
    dplyr::select(ID, NU_CUENCA, LON, LAT, NO_VERTIEN, CLUST, RANK)
  
  tsp_total <- tsp_length %>% sum()
  output <- list("rutas" = rutas, "tour_length" = tsp_length, "tour_total" = tsp_total)
  return(output)
}

# Se busca el método apropiado de generación de rutas
tsp_selection(rutas_data = map_data, pruebas = 100)

# Leer el Shapefile de Costa Rica
tz <- raster::getData('GADM', country = "CR", level = 1)
tz.prj <- sp::spTransform(tz, CRS("+proj=longlat +datum=WGS84 +no_defs")) 
tz <- fortify(tz.prj)

# Se generan las rutas con una inicialización seleccionada para comparar
# se utiliza el método de mejor ajuste según función tsp_selection()
set.seed(123)
tsp <- tsp_create(rutas_data = map_data, "farthest_insertion")
tsp$tour_length


# Plot de CR con sus rutas encontradas
ggplot() + 
  geom_polygon(data = tz.prj, aes(long, lat, group = group), 
               fill = "#f1f4c7", color = "#afb38a") +
  xlim(-86, -82.5) + ylim(8, 11.25) + 
  xlab("Longitud") + ylab("Latitud") + 
  coord_equal() + 
  geom_point(tsp$rutas, mapping = aes(LON, LAT, col = factor(CLUST))) + 
  #geom_point(new %>% filter(rank == max(rank)), mapping = aes(Longitud, Latitud), col = "red") + 
  geom_path(tsp$rutas[order(tsp$rutas$RANK),],
            mapping = aes(x = LON, y = LAT, group = factor(CLUST), col = factor(CLUST)), size = 1) +
  labs(color = "Ruta") +
  theme_bw() +
  theme(legend.position = "none", 
      axis.title = element_blank(), 
      axis.text = element_blank())

# Plot de mapa relativo si se tiene información en coordenadas
plot_dimred <- data.frame(tsp$rutas,map_data[1:3]) %>% as_tibble() 

ggplot() + 
  geom_point(plot_dimred, mapping = aes(V1, V2, col = factor(CLUST))) + 
  geom_path(plot_dimred[order(plot_dimred$RANK),],
            mapping = aes(x = V1, y = V2, group = factor(CLUST), col = factor(CLUST)), size = 1) +
  labs(color = "Ruta") +
  theme_bw() + 
  theme(legend.position = "none", 
      axis.title = element_blank(), 
      axis.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank())

# Función para obtener la distancia de rutas con los datos originales
tsp_length <- function(dist) {
  f <- function (i, j, dist_obj) {
    if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
    n <- attr(dist_obj, "Size")
    valid <- (i >= 1) & (j >= 1) & (i > j) & (i <= n) & (j <= n)
    k <- (2 * n - j) * (j - 1) / 2 + (i - j)
    k[!valid] <- NA_real_
    k
  }
  SliceExtract_dist <- function (dist_obj, k) {
    if (length(k) > 1) stop("The function is not 'vectorized'!")
    n <- attr(dist_obj, "Size")
    if (k < 1 || k > n) stop("k out of bound!")
    ##
    i <- 1:(k - 1)
    j <- rep.int(k, k - 1)
    v1 <- dist_obj[f(j, i, dist_obj)]
    ## 
    i <- (k + 1):n
    j <- rep.int(k, n - k)
    v2 <- dist_obj[f(i, j, dist_obj)]
    ## 
    c(v1, 0, v2)
  }
  
  num_rutas <- tsp$rutas %>% dplyr::select(CLUST) %>% max()
  a <- dist %>% as.matrix()
  tsp_value <- matrix(ncol = num_rutas)
  colnames(tsp_value) <- c(1:num_rutas)
  rownames(tsp_value) <- "tour"
  for (j in 1:num_rutas) {
    tsp_values = 0
    order_tsp <- tsp$rutas %>% filter(CLUST == j)
    order_tsp <- order_tsp[order(order_tsp$RANK),] %>% pull(ID)
    for (i in 1:(length(order_tsp)-1)) {
      search_row <- SliceExtract_dist(dist, which(colnames(a) == order_tsp[i]))
      search_col <- which(colnames(a) == order_tsp[i+1])
      tsp_values <- tsp_values + search_row[search_col]
    }
    tsp_value[j] <- tsp_values
  }
  output <- list("tsp_lengths" = tsp_value, "tsp_total" = sum(tsp_value))
  return(output)
}

# Longitud de ruta dada en metros o segundos
tsp_length(time_mat2)


