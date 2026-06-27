
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
