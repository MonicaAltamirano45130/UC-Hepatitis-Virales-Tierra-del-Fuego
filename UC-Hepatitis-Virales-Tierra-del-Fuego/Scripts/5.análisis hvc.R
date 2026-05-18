
#========================================
# 📊 RESUMEN CONFIRMACIÓN
#========================================

resumen_eventos <- data_C_completa %>%
  group_by(`Confirmado`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

resumen_eventos


#========================================
# 📊 CONFIRMADOS
#========================================

confirmados_c <- data_C_completa %>%
  filter(`Confirmado` == "SI")

confirmados_c_totales <- confirmados_c %>%
  summarise(Total = n())

confirmados_c_totales

#----------------------------------------
# CO-INFECCIÓN
#----------------------------------------

co_infección_c <- confirmados_c %>%
  group_by(`Co-infección`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

co_infección_c 


#----------------------------------------
# EDAD
#----------------------------------------

confirmados_gedad <- confirmados_c %>%
  group_by(`Grupo etario diagnóstico`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  adorn_totals(where = "row")

confirmados_gedad 


#----------------------------------------
# COMPLICACIONES
#----------------------------------------

confirmados_cirrosis <- confirmados_c %>%
  group_by(`Cirrosis`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

confirmados_hcc <- confirmados_c %>%
  group_by(`Hepatocarcinoma`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

confirmados_tx <- confirmados_c %>%
  group_by(`Transplante`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

confirmados_falle <- confirmados_c %>%
  group_by(`Fallecido`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

confirmados_perdidos <- confirmados_c %>%
  group_by(`Perdida de seguimiento`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

confirmados_tto <- confirmados_c %>%
  group_by(`Tratamiento...26`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")


#----------------------------------------
# RVS (solo tratados)
#----------------------------------------

confirmados_rvs <- confirmados_c %>%
  filter(`Tratamiento...26` == "SI")

confirmados_rvs <- confirmados_rvs %>%
  group_by(`Respuesta viral sostenida`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

confirmados_rvs


#========================================
# 📊 SIN DATOS
#========================================

sin_datos_c <- data_C_completa %>%
  filter(`Confirmado` == "SIN DATOS")

nrow(sin_datos_c)

sin_datos_c <- sin_datos_c %>%
  group_by(`Grupo de evento`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

sin_datos_c


#========================================
# 💾 EXPORTACIÓN
#========================================

write_xlsx(data_C_completa, "Salidas/data_C_completa.xlsx")
# write_csv(data_C_completa, "Salidas/data_C_completa.csv")
