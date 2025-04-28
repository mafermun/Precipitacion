library(tidyverse)
load("../Precipitación/data/pcp_ta_data.rda")
load("../AnalisisPlantas/output/tablas/data_clean.rda")
ggplot(pcp_ta_data, aes(x = fecha, y = pcp)) +
  geom_line()

pcp_diario <- pcp_ta_data %>% 
  group_by(fecha = floor_date(fecha, "day")) %>% 
  summarise(npcp = sum(!is.na(pcp)),
            ntam = sum(!is.na(tam)),
            ntan = sum(!is.na(tan)),
            ntax = sum(!is.na(tax)),
            pcp = sum(pcp, na.rm = TRUE),
            tam = mean(tam, na.rm = TRUE),
            tan = mean(tan, na.rm = TRUE),
            tax = mean(tax, na.rm = TRUE))

pcp_diario_est <- pcp_diario %>% 
  mutate(pcp = if_else(npcp > 0.60 * 24, pcp, NA_real_),
         tam = if_else(ntam < 0.60 * 24 | is.infinite(tam), NA_real_, tam),
         tan = if_else(ntan < 0.60 * 24 | is.infinite(tan), NA_real_, tan),
         tax = if_else(ntax < 0.60 * 24 | is.infinite(tax), NA_real_, tax))

ggplot(pcp_diario_est, aes(x = fecha, y = pcp)) +
  geom_line()

save(pcp_diario_est, file = "output/tablas/pcp_diario_est.rda")

dat_veg <- data_clean %>% 
  select(treatment,plot, SUBPARCELA, fecha) %>% 
  distinct() %>% 
  arrange(fecha) %>% 
  group_by(treatment,plot, SUBPARCELA) %>% 
  mutate(id = 1:n()) %>% 
  pivot_wider(id_cols = id,
              names_from = c(treatment,plot, SUBPARCELA),
              values_from = fecha)

### precipitacion unida fechas de vegetacion ----
pcp_veg_Drought_A_1 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_A_1)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))
  
pcp_veg_Drought_A_12 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_A_12)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Drought_A_16 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_A_16)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Drought_B_4 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_B_4)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Drought_B_5 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_B_5)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Drought_B_11 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_B_11)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Drought_C_1 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_C_1)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Drought_C_4 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_C_4)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Drought_C_5 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Drought_C_5)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Control_A_2 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_A_2)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Control_A_9 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_A_9)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Control_A_10 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_A_10)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Control_B_2 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_B_2)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Control_B_12 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_B_12)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Control_B_15 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_B_15)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))


pcp_veg_Control_C_1 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_C_1)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Control_C_4 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_C_4)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg_Control_C_12 <- pcp_diario %>% 
  select(fecha, pcp) %>% 
  left_join(select(dat_veg, id, fecha = Control_C_12)) %>% 
  fill(id, .direction = "up") %>% 
  group_by(id) %>% 
  summarise(n = sum(!is.na(pcp)),
            npd = sum(pcp = 0, na.rm = T),
            pd = sum(pcp > 0, na.rm = T),
            acu = sum(pcp, na.rm = T),
            max = max(pcp, na.rm = T),
            avg = mean(pcp, na.rm = T),
            min = min(pcp, na.rm = T),
            sd = sd(pcp, na.rm = T))

pcp_veg <- set_names(list(pcp_veg_Control_A_10, pcp_veg_Control_A_2, pcp_veg_Control_A_9, 
               pcp_veg_Control_B_12, pcp_veg_Control_B_15, pcp_veg_Control_B_2, 
               pcp_veg_Control_C_1, pcp_veg_Control_C_12, pcp_veg_Control_C_4, 
               pcp_veg_Drought_A_1, pcp_veg_Drought_A_12, pcp_veg_Drought_A_16, 
               pcp_veg_Drought_B_11, pcp_veg_Drought_B_4, pcp_veg_Drought_B_5, 
               pcp_veg_Drought_C_1, pcp_veg_Drought_C_4, pcp_veg_Drought_C_5
), 
nm = ls(pattern = "pcp_veg_")) %>% 
  bind_rows(.id = "grupo") %>% 
  mutate(grupo = str_remove(grupo, "pcp_veg_"))


load("../AnalisisPlantas/output/tablas/modelolineal_altura.rda")

slp_alt <- mdl_tidy_altura %>% 
  ungroup() %>% 
  # filter(term == "fecha") %>% 
  select(treatment, plot, SUBPARCELA, Planta, term, Slope) %>% 
  pivot_wider(id_cols = c(plot, treatment, SUBPARCELA, Planta),
              values_from = Slope,
              names_from = term) %>% 
  rename(alt_b0 = `(Intercept)`,
         alt_b1 = fecha)

load("../AnalisisPlantas/output/tablas/modelolineal_hojas.rda")

slp_data <- mdl_tidy_hojas %>% 
  ungroup() %>% 
  select(treatment, plot, SUBPARCELA, Planta, term, Slope) %>% 
  pivot_wider(id_cols = c(plot, treatment, SUBPARCELA, Planta),
              values_from = Slope,
              names_from = term) %>% 
  rename(hoj_b0 = `(Intercept)`,
         hoj_b1 = fecha) %>% 
  right_join(slp_alt, by = c("treatment", "plot", "SUBPARCELA", "Planta"))

cond_antecedentes_pcp <- dat_veg %>% 
  pivot_longer(-id, names_to = "grupo", values_to = "fecha") %>%
  left_join(pcp_veg, by = c("grupo", "id")) %>% 
  separate(grupo, into = c("treatment", "plot", "SUBPARCELA")) %>%
  mutate(SUBPARCELA = as.numeric(SUBPARCELA)) %>% 
  right_join(slp_data, by = c("treatment", "plot", "SUBPARCELA"), 
             relationship = "many-to-many") %>% 
  mutate(alt_chg = as.numeric(fecha) * alt_b1 + alt_b0,
         hoj_chg = as.numeric(fecha) * hoj_b1 + hoj_b0) %>% 
  filter(alt_b1 > 0) %>%
  group_by(treatment,plot, SUBPARCELA, acu) %>%
  summarise(alt_chg = median(alt_chg, na.rm = T),
            hoj_chg = median(hoj_chg, na.rm = T))
  # summarise(acu = mean(acu, na.rm = T),
  #           max = mean(max, na.rm = T),
  #           pcp = mean(avg, na.rm = T),
  #           min = mean(min, na.rm = T),
  #           sd = mean(sd, na.rm = T),
            # slp.hoj = mean(hoj_b1, na.rm = T),
            # slp.alt = mean(alt_chg, na.rm = T))

save(cond_antecedentes_pcp, file = "../AnalisisPlantas/output/tablas/cambioen_alturayhojas_precipitacion.rda")


cond_antecedentes_pcp %>% 
  # group_by(treatment, plot) %>% 
  # filter(alt_b1 < 0.2) %>%
  # filter(slp.alt < 0.2) %>% 
ggplot(aes(x = acu, y = alt_chg, color = factor(plot))) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)", se = F) +
  facet_wrap(~treatment, scales = "free")


lm()