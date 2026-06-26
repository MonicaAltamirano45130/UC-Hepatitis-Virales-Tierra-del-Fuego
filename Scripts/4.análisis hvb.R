
#========================================
# 📊 RESUMEN CLASIFICACIÓN
#========================================

resumen_eventos_b <- data_B_completa %>%
  mutate(
    `Clasificación` = case_when(
      `Clasificación` %in% c("Aguda", "Aguda y resuelta") ~ "Aguda",
      TRUE ~ `Clasificación`
    )
  ) %>%
  group_by(`Clasificación`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  adorn_totals(where = "row")

resumen_eventos_b

notificados_b_totales <- data_B_completa %>%
  summarise(Total = n())

notificados_b_totales

#========================================
# 📊 CASOS CONFIRMADOS
#========================================

confirmados_b <- data_B_completa %>%
  filter(`Clasificación` == "Aguda" |
           `Clasificación` == "Crónica" |
           `Clasificación` == "Resuelta" |
           `Clasificación` == "Aguda y resuelta")

nrow(confirmados_b)

confirmados_b_totales <- confirmados_b %>%
  summarise(Total = n())

confirmados_b_totales
#========================================
# 📊 CO-INFECCIÓN
#========================================

co_infección_b <- confirmados_b %>%
  group_by(`Co-infección`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

co_infección_b


#========================================
# 📊 AGUDOS
#========================================

aguda_resuelta <- confirmados_b %>%
  filter(`Clasificación` == "Aguda" |
           `Clasificación` == "Aguda y resuelta")

nrow(aguda_resuelta)

aguda_resuelta_gedad <- aguda_resuelta %>%
  group_by(`Grupo etario diagnóstico`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  adorn_totals(where = "row")

aguda_resuelta_gedad


#========================================
# 📊 CRÓNICOS
#========================================

cronica <- confirmados_b %>%
  filter(`Clasificación` == "Crónica")

nrow(cronica)

cronica_gedad <- cronica %>%
  group_by(`Grupo etario diagnóstico`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  adorn_totals(where = "row")

cronica_gedad


#----------------------------------------
# Complicaciones
#----------------------------------------

cronica_cirrosis <- cronica %>%
  group_by(`Cirrosis`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

cronica_hcc <- cronica %>%
  group_by(`Hepatocarcinoma`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

cronica_tx <- cronica %>%
  group_by(`Transplante`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

cronica_falle <- cronica %>%
  group_by(`Fallecido`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

cronica_perdidos <- cronica %>%
  group_by(`Pérdida de seguimiento`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

cronica_tto <- cronica %>%
  group_by(`Tratamiento...26`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")


#========================================
# 📊 PÉRDIDA DE SEGUIMIENTO (TOTAL)
#========================================
perdidos <- data_B_completa %>%
  filter (Clasificación== "Aguda" & `Pérdida de seguimiento`=="SI" )

perdidos <- perdidos %>%
  group_by(`Pérdida de seguimiento`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

perdidos


#========================================
# 📊 SIN DATOS
#========================================

sin_datos <- confirmados_b %>%
  filter(`Clasificación` == "Sin datos")

nrow(sin_datos)

sin_datos <- sin_datos %>%
  group_by(`Grupo de evento`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

sin_datos


#========================================
# 💾 EXPORTACIÓN
#========================================

write_xlsx(data_B_completa, "Salidas/data_B_completa.xlsx")
