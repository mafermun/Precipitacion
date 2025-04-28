library(tidyverse)
library(ggstatsplot)
source("R/ietd_rain_events.R")

### data estacion universidad----

load("../Precipitación/data/pcp_ta_data.rda")

data_pcp <- pcp_ta_data %>% 
  select(1:2)

### funcion

ietd_nev <- function(x, y, dat = data){
  e <- ietd_re(.x = dat, 
               ietd = x, 
               thres = y)[[1]] 
  tibble(ietd = x, thres = y, n = nrow(e))
}

### lista de eventos y caracteritsticas de precipitacion

eventos_pcp <- ietd_re(data_pcp,
                       ietd = as.difftime(8, units = "hours"), 
                       thres = 10)

# neventos_pcp <- ietd_nev(as.difftime(2 , units = "hours"),
#                          5,
#                          data_pcp)
# 
# evs_pcp <- 1

### todo el calculo

# comb_pcp <- expand_grid(ietd = as.difftime(seq(2.5, 24, by = 0.5), units = "hours"),
#                         thres = seq(2, 6, by = 0.1))
# 
# neventos_pcp_test <- map2_dfr(comb_pcp$ietd, comb_pcp$thres, ietd_nev,
#                               .progress = TRUE)
#### guardamos el trabajo aqui

# save(neventos_pcp_test, file = "../Precipitación/output/tablas/n_eventos_test_pcp.rda")

### aqui grafica los eventos de precipitacion

data_pcp |> 
  ggplot() +
  geom_rect(mapping = aes(xmin = start, xmax = end,
                          ymin = 0, ymax = Inf,
                          fill = "Evento"),
            data = eventos_pcp[[1]], color = NA, alpha = 0.5
  ) +
  geom_segment(aes(x = fecha, xend = fecha, 
                   y = 0, yend = pcp), 
               color = "steelblue") +
  scale_fill_manual(values = c("Evento" = "grey70")) +
  # coord_cartesian(xlim = as.POSIXct(c("1988-08-01", "1988-10-10"))) +
  theme_light()


evs_pcp <- 50:55

#### Graficas

data_pcp |> 
  group_by(fecha = floor_date(fecha, "hour")) %>% 
  summarise(pcp = sum(pcp, na.rm = TRUE)) %>% 
  ggplot() +
  geom_rect(mapping = aes(xmin = start, xmax = end,
                          ymin = 0, ymax = Inf,
                          fill = "Evento"),
            data = eventos_pcp[[1]], color = NA, alpha = 0.5) +
  geom_col(aes(x = fecha, y = pcp),
           fill = "steelblue") +
  # geom_segment(aes(x = datetime, xend = datetime,
  #                  y = 0, yend = rainfall),
  #              color = "steelblue") +
  scale_fill_manual(values = c("Evento" = "grey50")) +
  # coord_cartesian(xlim = range(c(floor_date(eventos[[1]][[2]][evs], "hour"),
  #                                ceiling_date(eventos[[1]][[3]][evs], "hour"))),
  #                 ylim = c(0,15)) +
  # coord_cartesian(xlim = as.POSIXct(c("2022-07-01 ", "2025-01-01"))) +
  theme_light() +
  labs(title = "Rainfall events M5147 Station")

### thresholds
# 
# neventos_pcp_test |> 
#   ggplot() + 
#   geom_point(mapping = aes(x = thres,
#                            y = n,
#                            color = factor(ietd)))
# neventos_pcp_test |> 
#   ggplot() + 
#   geom_point(mapping = aes(x = ietd,
#                            y = n, color = factor(thres)))
# 
# neventos_pcp_test |> 
#   ggplot() + 
#   geom_point(mapping = aes(x = ietd,
#                            y = n)) +
#   facet_wrap(~thres)
# 
# 
# neventos_pcp_test |> 
#   ggplot() + 
#   geom_point(mapping = aes(x = ietd,
#                            y = n))
# 
# neventos_pcp_test |> 
#   ggplot() + 
#   geom_line(mapping = aes(x = thres,
#                           y = n, color = factor(thres)))
# 
# 
# neventos_pcp_test |> 
#   ggplot() + 
#   geom_point(mapping = aes(x = ietd,
#                            y = thres,
#                            size = n))
### tabla de caracteristicas de la lluvia

rain_charac <- eventos_pcp[["Rainfall_Characteristics"]] %>% 
  mutate(no_rain = c(NA, start[-1] - end[-length(end)]))

save(rain_charac, eventos_pcp, data_pcp, file = "../Precipitación/output/tablas/rainfall_characteristics.rda") 

##### data pluviometro de las parcelas ----

load("../Precipitación/output/tablas/data_clean_pcp_parcelas.rda")

### funcion: tengo que poner en "dat =" el nombre de la tabla de datos que tenga en ese momento?

ietd_nev <- function(x, y, dat = data){
  e <- ietd_re(.x = dat, 
               ietd = x, 
               thres = y)[[1]] 
  tibble(ietd = x, thres = y, n = nrow(e))
}

### lista de eventos y caracteritsticas de precipitacion

eventos_pcp_f <- ietd_re(data_pcp_f,
                       ietd = as.difftime(8, units = "hours"), 
                       thres = 10)

neventos_pcp_f <- ietd_nev(as.difftime(2 , units = "hours"),
                         5,
                         data_pcp_f)

evs_pcp_f <- 1

### para determinar la mejor combinacion de thresholds y ietd,
# ya determinamos que son thesh = 10 y iedt = 8

# comb_pcp_f <- expand_grid(ietd = as.difftime(seq(2.5, 24, by = 0.5), units = "hours"),
#                         thres = seq(2, 6, by = 0.1))
# 
# neventos_pcp_f_test <- map2_dfr(comb_pcp_f$ietd, comb_pcp_f$thres, ietd_nev,
#                               .progress = TRUE)

### aqui grafica los eventos de precipitacion

data_pcp_f |> 
  ggplot() +
  geom_rect(mapping = aes(xmin = start, xmax = end,
                          ymin = 0, ymax = Inf,
                          fill = "Evento"),
            data = eventos_pcp_f[[1]], color = NA, alpha = 0.5
  ) +
  geom_segment(aes(x = fecha, xend = fecha, 
                   y = 0, yend = pcp_f), 
               color = "steelblue") +
  scale_fill_manual(values = c("Evento" = "grey70")) +
  # coord_cartesian(xlim = as.POSIXct(c("1988-08-01", "1988-10-10"))) +
  theme_light()

evs_pcp_f <- 50:55

#### Graficas

data_pcp_f |> 
  ggplot() +
  geom_rect(mapping = aes(xmin = start, xmax = end,
                          ymin = 0, ymax = Inf,
                          fill = "Evento"),
            data = eventos_pcp_f[[1]], color = NA, alpha = 0.5
  ) +
  geom_segment(aes(x = fecha, xend = fecha, 
                   y = 0, yend = pcp_f), 
               color = "steelblue") +
  # geom_segment(aes(x = datetime, xend = datetime,
  #                  y = 0, yend = rainfall),
  #              color = "steelblue") +
  scale_fill_manual(values = c("Evento" = "grey50")) +
  # coord_cartesian(xlim = range(c(floor_date(eventos[[1]][[2]][evs], "hour"),
  #                                ceiling_date(eventos[[1]][[3]][evs], "hour"))),
  #                 ylim = c(0,15)) +
  # coord_cartesian(xlim = as.POSIXct(c("2022-07-01 ", "2025-01-01"))) +
  theme_light() +
  labs(title = "Rainfall events forest")


### tabla caracteristicas de la lluvia

rain_charac_f <- eventos_pcp_f[["Rainfall_Characteristics"]] %>% 
  mutate(no_rain = c(NA, start[-1] - end[-length(end)]))

save(rain_charac_f, eventos_pcp_f, data_pcp_f, 
     file = "../Precipitación/output/tablas/rainfall_characteristics_f.rda")

#### tests ----

which_ev <- function(xini, xend, yini, yend){
  int_x <- interval(xini, xend)
  int_y <- interval(yini, yend)
  w <- which(int_overlaps(int_y, int_x))
  if(length(w) == 0) return(NA)
  
  return(w[1])
}

eventos_match <- mutate(rain_charac, 
                        ev_pluvio = map2_int(start, end, .f = which_ev, 
                                             yini = rain_charac_f$start, yend = rain_charac_f$end)) %>% 
  left_join(mutate(rain_charac_f, ev_pluvio = row_number()), by = "ev_pluvio") %>% 
  na.omit %>% 
  mutate(duration.x = as.numeric(duration.x),
         duration.y = as.numeric(duration.y))

ggplot(eventos_match) +
  aes(duration.x, duration.y,
      color = intensity.x) +
  geom_point() 

ggplot(eventos_match) +
  aes(volume.x, volume.y,
      color = duration.y) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  coord_equal()


### caracteristicas de eventos de precipitacion ----

ks.test(eventos_match$duration.x, eventos_match$duration.y, alternative = "two.sided")
ks.test(eventos_match$intensity.x, eventos_match$intensity.y,  alternative = "two.sided")
# para dos muestras p<0.05 va a rechazar la hipotesis nula
# alternative comprueba si la distrivucion acumulada de x es mayor o menor que la distribucion acumulada de y

evmatch_70 <- eventos_match %>% 
  filter(duration.x <= 70) s <- ks.test(eventos_match$duration.x, eventos_match$duration.y)

ks.test(eventos_match$intensity.x, eventos_match$intensity.y)

eventos_match %>% 
  filter(duration.x <= 70) %>% 
ggplot(aes(x = duration.x, y = duration.y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x")

eventos_match <- eventos_match %>% 
  filter(duration.x <= 70)

save(eventos_match, file = "../Precipitación/output/tablas/eventos_match70.rda")

theme_tesis <- function (...) 
{
  theme_bw(...) + 
    theme(axis.title = element_blank(), 
          plot.title = element_text(size = 10), 
          panel.border = element_blank(), 
          strip.text = element_text(size = 10),
          plot.subtitle = element_text(size = 7))
}

eventos_match %>% 
  select(duration.x, duration.y, intensity.x, intensity.y, 
         volume.x, volume.y, no_rain.x, no_rain.y) %>% 
  pivot_longer(everything(), 
               names_to = c("Variable", "Sitio"), 
               names_pattern = "(.*).(.)",
               values_to = "Valor") %>% 
  mutate(Sitio = recode(Sitio, "x" = "Estación", "y" = "Bosque"),
         Variable = recode(Variable,
                           duration = "Duración (h)",
                           intensity = "Intensidad (mm/h)",
                           no_rain = "Horas sin lluvia",
                           volume = "Volumen (mm)")) %>% 
  grouped_ggbetweenstats(Sitio, `Valor`, pairwise.display = "all",
                         type = "nonparametric",
                 grouping.var = Variable,
                 
                 ggtheme = theme_tesis(),
                 centrality.label.args = list(size = 2, nudge_x = 0.4, segment.linetype = 4,
                                              min.segment.length = 0),
                 point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
                                     0.4, size = 2, stroke = 0, na.rm = TRUE)) 