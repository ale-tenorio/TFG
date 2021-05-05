library(tidyr)
library(dplyr)
library(stringr)

#Se ingresa la información en formato .csv de Carretera, Tiempo y distancias Euclídeas
CARRETERA = "D:/QGis/Capas nuevas/DATOS REALES/distancias reales.csv"
TIEMPO =  "D:/U/DATOS/Time matrix.csv"
ESTACIONES = "D:/QGis/Capas nuevas/DATOS REALES/Estaciones.csv"

#Generación de matriz simétrica de distancias en carretera
df.road <- read.csv(file = CARRETERA,header = TRUE,sep = ";") %>% as_tibble()

lista_borrar <- df.road %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(A = as.character(origin_id),
                B = as.character(destination_id),
                d = total_cost) %>% 
  dplyr::select(A,B,d) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(is.na(d)) %>% 
  pull(A)

Estaciones <- read.csv(file = ESTACIONES,header = TRUE,sep = ",") %>% 
  filter(!(ID %in% lista_borrar)) %>% 
  as_tibble()

road_mat <- df.road %>% 
  dplyr::mutate(A = as.character(origin_id),
                B = as.character(destination_id),
                d = total_cost) %>% 
  dplyr::select(A,B,d) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(! (A %in% lista_borrar | B %in% lista_borrar)) %>% 
  pivot_wider(names_from = B, values_from = d) %>% 
  tibble::column_to_rownames(var="A") %>% 
  as.dist()

#Generación de matriz simétrica de tiempos de viaje
#time_mat, time_mat2, time_mat3 <- escenarios propuestos
df.time <- read.csv(file = TIEMPO,header = TRUE,sep = ";") %>%
  dplyr::as_tibble()
colnames(df.time)[1] <- "INICIO"

df.time <- df.time %>% 
  dplyr::mutate(A = as.character(INICIO),
                B = as.character(DESTINO),
                d = duracion) %>% 
  dplyr::select(A,B,d) %>%  as_tibble()

lista_borrar_2 <- df.time %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(is.na(d)) %>% group_by(A) %>% dplyr::count() %>% 
  filter(n > 18) %>% pull(A)

Estaciones_2 <- read.csv(file = ESTACIONES,header = TRUE,sep = ",") %>% 
  filter(!(ID %in% lista_borrar_2)) %>% 
  dplyr::mutate(TIPO = ifelse(grepl("-",ID),"HIDRO","METEORO")) %>%
  as_tibble()

time_mat <-  df.time %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(! (A %in% lista_borrar_2 | B %in% lista_borrar_2)) %>% 
  pivot_wider(names_from = B, values_from = d) %>% 
  tibble::column_to_rownames(var="A") %>% 
  as.matrix.data.frame()

time_mat <- ((time_mat+t(time_mat))/2) %>% as.dist()

time_mat2 <-  df.time %>% 
  dplyr::mutate(d = ifelse(d >= 8*(3600),d + 12*(3600),d)) %>% 
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(! (A %in% lista_borrar_2 | B %in% lista_borrar_2)) %>% 
  pivot_wider(names_from = B, values_from = d) %>% 
  tibble::column_to_rownames(var="A") %>% 
  as.matrix.data.frame() 

time_mat2 <- ((time_mat2+t(time_mat2))/2) %>% as.dist()

time_mat3 <-  df.time %>% 
  dplyr::mutate(d = ifelse(grepl("-",B),d + 3*(3600),d + 1*(3600))) %>%
  dplyr::mutate(d = ifelse(A==B, 0, d)) %>% 
  filter(! (A %in% lista_borrar_2 | B %in% lista_borrar_2)) %>% 
  pivot_wider(names_from = B, values_from = d) %>% 
  tibble::column_to_rownames(var="A") %>% 
  as.matrix.data.frame() 

time_mat3 <- ((time_mat3+t(time_mat3))/2) %>% as.dist()

#Generación de matriz simétrica de distancias euclideas
Estaciones_3 <- read.csv(file = ESTACIONES,header = TRUE,sep = ",") %>% 
  as_tibble()

euc_mat <- Estaciones_3 %>% 
  #filter(!ID %in% lista_borrar_2) %>% 
  dplyr::select(X_CRTM05,Y_CRTM05) %>% 
  dist() 


