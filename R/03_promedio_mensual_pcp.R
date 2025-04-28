library(tidyverse)

load("../Precipitación/data/pcp_ta_data.rda")
# precipitacion promedio anual durante el experimento de la estacion
pcp_ta_data %>%
  group_by(fecha = floor_date(fecha, "day")) %>%
  summarise(pcp = sum(pcp, na.rm = T)) %>%
  group_by(fecha = floor_date(fecha, "month")) %>%
  summarise(mpcp = sum(pcp, na.rm = T),
            .groups = "drop") %>% 
  filter(mpcp >0) %>% 
  summarise(pcp = mean(mpcp))

# pcp_ta_data %>% 
#   group_by(fecha = floor_date(fecha, "day")) %>% 
#   summarise(n = sum(!is.na(pcp)),
#             pcp = sum(pcp, na.rm = T)) %>% 
#   filter(n > 0.6 * 24) %>% 
#   group_by(fecha = floor_date(fecha, "month")) %>% 
#   summarise(n = sum(!is.na(pcp)),
#             pcp = sum(pcp, na.rm = T)) %>% 
#   filter(n > 0.6 * 30) %>% 
#   group_by(fecha = month(fecha)) %>% 
#   summarise(pcp = mean(pcp, na.rm = T),
#             .groups = "drop") %>%
#   summarise(pcp = sum(pcp))

# precipitacion promedio anual durante el experimento del bosque

load("../Precipitación/output/tablas/data_clean_pcp_parcelas.rda")

data_pcp_f %>%
  group_by(fecha = floor_date(fecha, "day")) %>%
  summarise(pcp_f = sum(pcp_f, na.rm = T)) %>%
  group_by(fecha = floor_date(fecha, "month")) %>%
  summarise(pcp_f = sum(pcp_f, na.rm = T),
            .groups = "drop") %>% 
  filter(pcp_f >0) %>% 
  summarise(pcp = mean(pcp_f))


