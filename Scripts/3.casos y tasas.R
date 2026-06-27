# Cuento los casos notificados en SNVS

notificados_totales <- data %>%
  filter(Evento %in% c("Hepatitis A", "Hepatitis B", "Hepatitis C")) %>%
  summarise(Total = n())

# Cuento los casos confirmados SNVS

confirmados_año <- data %>%
  filter(`Clasificación manual del caso`=="Virus Hepatitis C, genotipo 1b"|
           `Clasificación manual del caso`=="05. Caso CONFIRMADO de Infección Crónica por VHB"|
           `Clasificación manual del caso`=="Virus Hepatitis C, genotipo 1a"|
           `Clasificación manual del caso`== "04. Caso CONFIRMADO de Infección por VHC"|
           `Clasificación manual del caso`=="12. Virus Hepatitis B"|
           `Clasificación manual del caso`=="04. Caso CONFIRMADO de Infección por VHB"|
           `Clasificación manual del caso`=="Virus Hepatitis C, genotipo 1"|
           `Clasificación manual del caso`=="05. Infección resuelta o falso anti VHC"|
           `Clasificación manual del caso`=="06. Caso CONFIRMADO de Infección Aguda por VHB"|
           `Clasificación manual del caso`=="09. Caso CONFIRMADO de Infección OCULTA por VHB"|
           `Clasificación manual del caso`=="Virus Hepatitis C, genotipo 3"|
           `Clasificación manual del caso`=="Virus Hepatitis C, genotipo 4"
  )

confirmados_totales <- confirmados_año %>%
    summarise(Total = n())

# Selecciono la variable de tiempo: fecha de recolección en papel

confirmados_año <- confirmados_año %>%
  select(Evento, `fecha_recoleccion`,`Clasificación manual del caso`,`año_papel`) %>%
  mutate(
    fecha = parse_date_time(`fecha_recoleccion`, orders = c("ymd", "dmy", "mdy")),
    anio = year(fecha),
    semana = isoweek(fecha)
  )

# Uno la base con población para calular las tasas

confirmados_año_poblacion <- left_join(confirmados_año, poblacion, by = "anio")

# Agrupo los casos confirmados por año

confirmados_año_poblacion <- confirmados_año_poblacion %>%
  group_by(anio, `Evento`) %>%
  summarise(casos = n(), .groups = "drop")

confirmados_año_poblacion <- confirmados_año_poblacion %>%
  pivot_wider(
    names_from = `Evento`,
    values_from = casos,
    values_fill = 0
  )
# Armo la tabla final de casos y tasas

tabla_final <- confirmados_año_poblacion %>%
  mutate(`Total_HV` = rowSums(across(-anio))) %>%
  left_join(poblacion, by = "anio") %>%
  mutate(
    TASA_100MIL_HVB = round((`Hepatitis B` / poblacion) * 100000, 1),
    TASA_100MIL_HVC = round((`Hepatitis C` / poblacion) * 100000, 1)
  ) %>%
  arrange(anio)

tabla_final_gt <- tabla_final %>%
  arrange(anio) %>%
  gt() %>%
  cols_hide(columns = poblacion) %>%
  cols_move(
    columns = c(`Hepatitis B`,TASA_100MIL_HVB, `Hepatitis C`,TASA_100MIL_HVC, Total_HV,
    ),
    after = anio
  ) %>%
  cols_label(
    anio = "Año",
    `Hepatitis B` = "HVB",
    TASA_100MIL_HVB = "Tasa HVB",
    `Hepatitis C` = "HVC",
    TASA_100MIL_HVC = "Tasa HVC",
    Total_HV = "Total HV/Año"
  ) %>%
  cols_align(align = "center", -anio) %>%
  fmt_number(
    columns = c(TASA_100MIL_HVB, TASA_100MIL_HVC),
    decimals = 1
  ) %>%
  tab_header(
    title = "Casos y tasas de HV en Tierra del Fuego",
    subtitle = "Período 2018–2025"
  )


#========================================
# 📌 Preparar datos
#========================================
df_plot <- tabla_final %>%
  arrange(anio)

#========================================
# 📊 Gráfico combinado
#========================================
curva_interactiva_casos <- highchart() %>%
  
  # Tipo de gráfico
  hc_chart(zoomType = "x") %>%
  
  # Títulos
  hc_title(text = "Casos y tasas de Hepatitis B y C") %>%
  hc_subtitle(text = "Tierra del Fuego, 2018–2025") %>%
  
  # Eje X
  hc_xAxis(categories = df_plot$anio) %>%
  
  # Ejes Y
  hc_yAxis_multiples(
    list(title = list(text = "Casos")),
    list(title = list(text = "Tasas por 100.000"), opposite = TRUE)
  ) %>%
  
  #========================================
# 📊 CASOS (columnas)
#========================================
hc_add_series(
  name = "Casos HVB",
  type = "column",
  data = df_plot$`Hepatitis B`,
  yAxis = 0
) %>%
  
  hc_add_series(
    name = "Casos HVC",
    type = "column",
    data = df_plot$`Hepatitis C`,
    yAxis = 0
  ) %>%
  
  #========================================
# 📈 TASAS (líneas)
#========================================
hc_add_series(
  name = "Tasa HVB",
  type = "spline",
  data = df_plot$TASA_100MIL_HVB,
  yAxis = 1
) %>%
  
  hc_add_series(
    name = "Tasa HVC",
    type = "spline",
    data = df_plot$TASA_100MIL_HVC,
    yAxis = 1
  ) %>%
  
  #========================================
# 🎨 Opciones
#========================================
hc_plotOptions(
  column = list(
    grouping = TRUE,
    shadow = FALSE,
    borderWidth = 0
  )
) %>%
  
  hc_tooltip(shared = TRUE) %>%
  hc_legend(enabled = TRUE)

curva_interactiva_casos

