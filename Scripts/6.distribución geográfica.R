#========================================
# 📊 NOTIFICADOS POR LOCALIDAD
#========================================

notificados_localidad_b <- data_B_completa %>%
  group_by(`Localidad Residencia`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

notificados_localidad_b


notificados_localidad_c <- data_C_completa %>%
  group_by(`Localidad Residencia`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

notificados_localidad_c


#========================================
# 📊 NOTIFICADOS POR GRUPO ETARIO
#========================================

notificados_gedad <- data %>%
  group_by(`Grupo etario diagnóstico`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  adorn_totals(where = "row")

notificados_gedad


#========================================
# 📊 NOTIFICADOS POR SEXO
#========================================

notificados_sexo <- data %>%
  group_by(`Sexo Legal`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Casos)) %>%
  adorn_totals(where = "row")

notificados_sexo