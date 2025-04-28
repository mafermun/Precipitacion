library(tidyverse)
library(dplyr)
library(factoextra)
library(NbClust)
library(tibble)
library(gridExtra)

load("../Precipitación/output/tablas/eventos_match70.rda")

estacion <- eventos_match %>% 
  select(duration.x, volume.x, intensity.x, no_rain.x) %>%  
  mutate(across(everything(),
                scale)) 

# fviz_nbclust(estacion, kmeans,
#              method = "wss") +
#   labs(title = "gap - num opt de conglomerados est")
# 
# set.seed(1)
# 
# clus_estacion <- NbClust(estacion, distance = "euclidean",
#                          min.nc = 2, max.nc = 10, 
#                          method = "kmeans", index ="all")

set.seed(1)

mdl_clusters.x <- kmeans(estacion, 7)

#### modelo para punto y ----

bosque <- eventos_match %>% 
  filter(no_rain.y <= 1000) %>% 
  select(duration.y, volume.y, intensity.y, no_rain.y) %>%  
  mutate(across(everything(),
                scale)) 

set.seed(1)
mdl_clusters.y <- kmeans(bosque, 7)

#### juntar clusters ----

clusters.estacion <- eventos_match %>% 
  select(duration = duration.x, 
         volume = volume.x, 
         intensity = intensity.x, 
         no_rain = no_rain.x) %>% 
  mutate(cluster.x = mdl_clusters.x$cluster) %>% 
  group_by(sitio = "Estación", cluster.x) %>% 
  summarise(across(c(duration, volume, intensity, no_rain),
                   list(avg = mean, sd = sd)),
            n = n()) %>% 
  mutate(across(duration_avg:no_rain_sd, ~round(.x, 1))) %>% 
  transmute(sitio = sitio,
            cluster = sprintf("Categoría %d", rank(intensity_avg)),
            a1_numero_eventos = as.character(n),
            a2_Duracion = sprintf("%2.1f (±%2.1f)", duration_avg, duration_sd),
            a3_Volumen = sprintf("%2.1f (±%2.1f)", volume_avg, volume_sd),
            a4_Intensidad = sprintf("%2.1f (±%2.1f)", intensity_avg, intensity_sd),
            a5_Sin_precipitacion = sprintf("%3.1f (±%2.1f)", no_rain_avg, no_rain_sd)
            ) %>% 
  mutate(across(a2_Duracion:a5_Sin_precipitacion, ~str_pad(.x, pad = " ", width = 14)))

clusters.bosque <- eventos_match %>% 
  filter(no_rain.y <= 1000) %>% 
  select(duration = duration.y, 
         volume = volume.y, 
         intensity = intensity.y, 
         no_rain = no_rain.y) %>% 
  mutate(cluster = mdl_clusters.y$cluster) %>% 
  group_by(sitio = "Bosque", cluster) %>% 
  summarise(across(c(duration, volume, intensity, no_rain),
                   list(avg = mean, sd = sd)),
            n = n()) %>% 
  mutate(across(duration_avg:no_rain_sd, ~round(.x, 1))) %>% 
  transmute(sitio = sitio,
            cluster = sprintf("Categoría %d", rank(intensity_avg)),
            a1_numero_eventos = as.character(n),
            a2_Duracion = sprintf("%2.1f (±%2.1f)", duration_avg, duration_sd),
            a3_Volumen = sprintf("%2.1f (±%2.1f)", volume_avg, volume_sd),
            a4_Intensidad = sprintf("%2.1f (±%2.1f)", intensity_avg, intensity_sd),
            a5_Sin_precipitacion = sprintf("%3.1f (±%2.1f)", no_rain_avg, no_rain_sd)
  ) %>% 
  mutate(across(a2_Duracion:a5_Sin_precipitacion, ~str_pad(.x, pad = " ", width = 14)))

categorias_evpcp <- bind_rows(clusters.estacion, clusters.bosque) %>% 
  arrange(cluster) %>% 
  pivot_longer(a1_numero_eventos:a5_Sin_precipitacion, 
               names_to = "Variable", values_to = "texto") %>% 
  pivot_wider(id_cols = c(sitio, Variable), 
              names_from = cluster, values_from = texto) %>% 
  select(Variable, sitio, everything()) %>% 
  arrange(Variable, sitio) %>% 
  mutate(Variable = str_remove(Variable, "a[1-5]_"))

save(clusters.estacion, file = "output/tablas/cluster_estacion.rda")
save(clusters.bosque, file = "output/tablas/cluster_bosque.rda")
save(categorias_evpcp, file = "output/tablas/categorias_evpcp.rda")

ggplot(eventos_match, aes(x = factor(start.x) ,y = duration.x)) + 
  geom_col()

########

pcpc_f_clusters <- eventos_match %>% 
  filter(no_rain.y <= 1000) %>%
  select(ends_with("y")) %>% 
  mutate(cluster = mdl_clusters.y$cluster) %>% 
  bind_rows(filter(eventos_match, no_rain.y > 1000) %>% 
              select(ends_with("y")) %>% 
              mutate(cluster = 4)) %>% 
  arrange(event.y)

save(pcpc_f_clusters, file = "../Precipitación/output/tablas/clusters_debosque.rda")

