
fallecidos_hvc <- data_C_completa %>%
  select(`Nombre Ciudadano`,
         `Apellido Ciudadano`,
         `Nro Doc`,
    `Fallecido`,
    `Grupo etario diagnóstico`,
    `Sexo Legal`,
    `Co-infección`,
    `Confirmado`,
    `Cirrosis`,
    `Hepatocarcinoma`
  )

fallecidos_hvc <- fallecidos_hvc %>%
  filter(Fallecido=="SI")

letalidad_hvc

confirmados_gedad_falle <- fallecidos_hvc %>%
  group_by(`Grupo etario diagnóstico`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  adorn_totals(where = "row")

confirmados_gedad_falle 

co_infección_c_falle <- fallecidos_hvc %>%
  group_by(`Co-infección`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

co_infección_c_falle 

cirrosis_c_falle <- fallecidos_hvc %>%
  group_by(`Cirrosis`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

cirrosis_c_falle 

hcc_c_falle <- fallecidos_hvc %>%
  group_by(`Hepatocarcinoma`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100,1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

hcc_c_falle 

