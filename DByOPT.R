library(dbscan)
library(ggforce)
library(dplyr)
source("t-SNE.R")
source("Disimilaridades.R")

# Se ingresa el resultado que se quiera agrupar
df <- res_tsne
x <- df %>% dplyr::select(V1,V2,V3)

# DBSCAN

# Se escogen los parámetros eps y minPts que se ajusten mejor a los datos
db<- dbscan(x, eps = 4, minPts = 50)
# Plot incial de DBSCAN
hullplot(x, db, main = "DBSCAN")

# Visualización de DBSCAN por medio de ggplot()
db_plot <- data.frame(x,db$cluster) %>% as_tibble()

hull <- db_plot %>%
  group_by(db.cluster) %>%
  dplyr::slice(chull(V3,V2))

ggplot() +
  geom_point(db_plot %>% filter(!db.cluster == 0), mapping = aes(x=V3, y=V2, colour = as.factor(db.cluster)))+
  geom_point(db_plot %>% filter(db.cluster == 0), mapping = aes(x=V3, y=V2), col = "black")+
  aes(fill = factor(db.cluster))+
  geom_polygon(hull %>% filter(!db.cluster == 0), mapping = aes(x=V3,y=V2,colour = as.factor(db.cluster)),alpha = 0.1) +
  theme_bw() +
  guides(fill = FALSE) +
  labs(color = "Cluster")+
  xlab("V1") + ylab("V2")+
  theme(legend.position = "none", 
        axis.title = element_blank(), 
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# OPTICS

# Se corre por primera vez OPTICS para minPts escogido
opt <- optics(x, minPts = 20)
# Visualización inicial de OPTICS
plot(opt)

# Función para definir clusters a partir del gráfico de accesibilidad,
# el valor lim define la ubicación donde se separan los puntos ruido
optics_reach <- function(lim = 0) {
  
  n = length(opt$order)
  reach <- matrix(ncol = 4,nrow = n)
  colnames(reach) <- c("Order","Position","Reachability","Cluster")
  
  for (i in 1:n) {
    reach[i,1] <- i
    reach[i,2] <- opt$order[i]
    reach[i,3] <- opt$reachdist[opt$order[i]]
  }
  clust = 1
  for (i in 1:n) {
    if (i == 1) {reach[i,4] = clust; next}
    if (i == n && reach[i,3] > lim) {reach[i,4] = NA; next}
    if(reach[i,3] <= lim) {
      
      reach[i,4] = clust
      
    } else if (reach[i,3] > lim && reach[i+1,3] <= lim) {
      reach[i,4] = NA
      clust = clust + 1
    } else { 
      reach[i,4] = NA
    }
  }
  info <- reach %>% as_tibble()
  plot <- ggplot(info, aes(x = Order, y = Reachability, fill = as.factor(Cluster))) +
    geom_bar(stat = "identity", width = 0.5) + 
    coord_cartesian(ylim=c(floor(min(info$Reachability)),ceiling(max(info$Reachability[which(is.finite(info$Reachability))])*2)/2)) +
    geom_hline(yintercept = lim, col = "red") +
    guides(fill = FALSE) +
    theme_bw()
  
  return(list("info" = info, "plot" = plot))
}

# Función para definir clusters a partir del gráfico de accesibilidad,
# se modifica el criterio de elección por valores alpha y beta 
#(bloque unitario)
optics_test <- function(alpha = 0, beta = 0) {
  reach = 0
  n = length(opt$order)
  reach <- tibble(Order = c(1:n),
                  Position = opt$order,
                  Reachability = opt$reachdist[opt$order],
                  Cluster = numeric(1),
                  Dif_izq = Reachability - lag(Reachability),
                  Detect_izq = "")
  
  indicador = FALSE
  for (i in 1:n) {
    if (i == 1) {reach$Detect_izq[i] = indicador; next}
    if (reach$Detect_izq[i-1] == FALSE && reach$Dif_izq[i] <= 0) {
      reach$Detect_izq[i] = indicador
    } else if (reach$Detect_izq[i-1] == FALSE && reach$Dif_izq[i] > 0) {
      indicador = TRUE
      reach$Detect_izq[i] = indicador
    } else if (reach$Detect_izq[i-1] == TRUE && reach$Dif_izq[i] >= 0) {
      reach$Detect_izq[i] = indicador
    } else {
      indicador = FALSE
      reach$Detect_izq[i] = indicador
    }
  }
  
  reach <- reach %>% purrr::map_df(rev) %>% mutate(Dif_der = Reachability - lag(Reachability),
                                                   Detect_der = "")
  indicador = FALSE
  for (i in 1:n) {
    if (i == 1) {reach$Detect_der[i] = indicador; next}
    if (reach$Detect_der[i-1] == FALSE && reach$Dif_der[i] <= 0) {
      reach$Detect_der[i] = indicador
    } else if (reach$Detect_der[i-1] == FALSE && reach$Dif_der[i] > 0) {
      indicador = TRUE
      reach$Detect_der[i] = indicador
    } else if (reach$Detect_der[i-1] == TRUE && reach$Dif_der[i] >= 0) {
      reach$Detect_der[i] = indicador
    } else {
      indicador = FALSE
      reach$Detect_der[i] = indicador
    }
  }
  reach <- reach %>% purrr::map_df(rev) %>% 
    mutate(PEAK = ifelse((Detect_izq == TRUE) & (Detect_der == TRUE),TRUE,FALSE)) %>% 
    dplyr::select(Order, Position, Reachability, PEAK, Cluster) 
  
  clust = 1
  for (i in 1:n) {
    if (reach$PEAK[i] == FALSE) {
      reach$Cluster[i] = clust
    } else if (reach$PEAK[i] == TRUE && reach$PEAK[i-1] == FALSE) {
      clust = clust + 1
      reach$Cluster[i] = clust
    } else {
      reach$Cluster[i] = clust
    }
  }
  
  reach2 <- reach 
  reach2[mapply(is.infinite, reach2)] <- NA
  
    res.reach <- reach2 %>% dplyr::group_by(Cluster) %>% 
      summarise(first_rch = first(na.omit(Reachability)),
                min_rch = min(na.omit(Reachability)),
                last_rch = last(na.omit(Reachability)),
                first_pos = first(Order),
                min_pos = Order[which.min(Reachability)],
                min_posK = which.min(Reachability),
                last_pos = last(Order),
                N_min = sum(round(Reachability, 2) == round(min(Reachability,na.rm = TRUE), 2), na.rm = TRUE),
                countK= n()) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(min_pos2 = min_posK + floor(N_min/2),
                  b = min_pos2/countK,
                  d = 1 - b,
                  a = ifelse(first_rch > last_rch,1,(first_rch-min_rch)/(last_rch-min_rch)),
                  c = ifelse(last_rch > first_rch,1,(last_rch-min_rch)/(first_rch-min_rch)),
                  alpha = ifelse(is.na(atan(a/b)*180/pi),0,atan(a/b)*180/pi),
                  beta = ifelse(is.na(atan(c/d)*180/pi),0,atan(c/d)*180/pi)) %>% 
    dplyr::select(Cluster,a,c,b,d,alpha,beta)
  
  num <- dim(res.reach)[1] 
  for (i in 2:num-1) {
    if (res.reach$alpha[i] >= alpha && res.reach$beta[i] >= beta) {
      next
    } else if (res.reach$a[i] < res.reach$c[i]) {
      res.reach[i:num,1] <-  res.reach[i:num,1] - 1
    } else {
      res.reach[i,1] <- res.reach[i,1] + 1
      res.reach[i:num,1] <-  res.reach[i:num,1] - 1
    }
  }
  join <- res.reach %>% tibble::rownames_to_column('Clust_init') %>% transform(Clust_init = as.numeric(Clust_init)) %>% 
    dplyr::select(Cluster, Clust_init)
  
  info <- inner_join(reach, join, by = c("Cluster" = "Clust_init")) %>% 
    mutate(Cluster_old = Cluster,
           Cluster = Cluster.y) %>% 
    dplyr::select(Order, Position, Reachability, PEAK, Cluster_old, Cluster)
  
  plot <- ggplot(info, aes(x = Order, y = Reachability, fill = as.factor(Cluster))) +
    geom_bar(stat = "identity", width = 0.5) + 
    coord_cartesian(ylim=c(floor(min(info$Reachability)),ceiling(max(info$Reachability[which(is.finite(info$Reachability))])*2)/2)) +
    guides(fill = FALSE) +
    theme_bw()
  
  output = list("info" = info, "Resumen" = res.reach, "plot" = plot)
  return(output)
}

# OPTICS ORIGINAL
# Diagrama de accesibilidad separado
optics_reach(2)$plot
res <- optics_reach(2)$info

# Número de puntos ruido encontrados
sum(is.na(res$Cluster))

# Visualización de OPTICS original con ggplot()
optics_plot <- data.frame(df,CLUST = res[order(res$Position),]$Cluster) %>% as_tibble()

hull <- optics_plot %>%
  group_by(CLUST) %>%
  dplyr::slice(chull(V3,V2))

ggplot() +
  geom_point(optics_plot, mapping = aes(x=V3, y=V2, colour = as.factor(CLUST)))+
  aes(fill = factor(CLUST))+
  geom_polygon(hull %>% filter(!is.na(CLUST)), mapping = aes(x=V3,y=V2,colour = as.factor(CLUST)),alpha = 0.1) +
  theme_bw() +
  guides(fill = FALSE) +
  labs(color = "Cluster")+
  xlab("V1") + ylab("V2")+
  theme(legend.position = "none", 
        axis.title = element_blank(), 
        #axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# Geometría de bloques unitarios generados y diagrama de accesibilidad
optics_test(beta = 20)
res <- optics_test(beta = 20)$info

# Visualización de OPTICS modificado con ggplot()
optics_plot <- data.frame(df,CLUST = res[order(res$Position),]$Cluster) %>% as_tibble()

hull <- optics_plot %>%
  group_by(CLUST) %>%
  dplyr::slice(chull(V3,V2))

ggplot() +
  geom_point(optics_plot, mapping = aes(x=V3, y=V2, colour = as.factor(CLUST)))+
  aes(fill = factor(CLUST))+
  geom_polygon(hull %>% filter(!is.na(CLUST)), mapping = aes(x=V3,y=V2,colour = as.factor(CLUST)),alpha = 0.1) +
  theme_bw() +
  guides(fill = FALSE) +
  labs(color = "Cluster")+
  xlab("V1") + ylab("V2")+
  theme(legend.position = "none", 
        axis.title = element_blank(), 
        #axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())









