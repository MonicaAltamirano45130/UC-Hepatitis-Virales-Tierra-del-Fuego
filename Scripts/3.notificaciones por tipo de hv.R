
notificados_totales <- data %>%
  filter(Evento %in% c("Hepatitis A", "Hepatitis B", "Hepatitis C")) %>%
  summarise(Total = n())

notificados_totales
#========================================
# 📊 CASOS CONFIRMADOS POR AÑO
#========================================

#----------------------------------------
# 1. Filtrado de casos confirmados
#----------------------------------------
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

confirmados_totales

#----------------------------------------
# 2. Selección y variables de tiempo
#----------------------------------------
confirmados_año <- confirmados_año %>%
  select(Evento, `fecha_recoleccion`,`Clasificación manual del caso`,`año_papel`) %>%
  mutate(
    fecha = parse_date_time(`fecha_recoleccion`, orders = c("ymd", "dmy", "mdy")),
    anio = year(fecha),
    semana = isoweek(fecha)
  )


#----------------------------------------
# 3. Unión con población
#----------------------------------------
confirmados_año_poblacion <- left_join(confirmados_año, poblacion, by = "anio")

#----------------------------------------
# 4. Resumen anual
#----------------------------------------
confirmados_año_poblacion <- confirmados_año_poblacion %>%
  group_by(anio, `Evento`) %>%
  summarise(casos = n(), .groups = "drop")

#----------------------------------------
# 5. Formato ancho
#----------------------------------------
confirmados_año_poblacion <- confirmados_año_poblacion %>%
  pivot_wider(
    names_from = `Evento`,
    values_from = casos,
    values_fill = 0
  )

#----------------------------------------
# 6. Tabla final + tasas
#----------------------------------------
tabla_final <- confirmados_año_poblacion %>%
  mutate(`Total_HV` = rowSums(across(-anio))) %>%
  left_join(poblacion, by = "anio") %>%
  mutate(
    TASA_100MIL_HV = round((`Total_HV` / poblacion) * 100000, 1),
    TASA_100MIL_HVB = round((`Hepatitis B` / poblacion) * 100000, 1),
    TASA_100MIL_HVC = round((`Hepatitis C` / poblacion) * 100000, 1)
  ) %>%
  arrange(anio)

# Exportación
write_xlsx(tabla_final, "Salidas/tabla_resumen.xlsx")

tabla_final
#========================================
# 📊 PROPORCIONES SEMANALES
#========================================

notificaciones_prop <- confirmados_año %>%
  count(anio, semana, Evento) %>%
  complete(
    anio = 2000:2025,
    semana = 1:53,
    Evento,
    fill = list(n = 0)
  ) %>%
  group_by(anio, semana) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    SEPI = paste0(`anio`, "-", sprintf("%02d", `semana`))
  ) %>%
  arrange(anio, semana) %>%
  mutate(SEPI = factor(SEPI, levels = unique(SEPI)))

#----------------------------------------
# 📈 Gráfico semanal
#----------------------------------------
ggplot(notificaciones_prop,
       aes(x = SEPI, y = prop, fill = Evento)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    breaks = levels(notificaciones_prop$SEPI)[seq(1, length(levels(notificaciones_prop$SEPI)), by = 6)]
  ) +
  labs(
    title = "Proporción de hepatitis confirmadas por semana epidemiológica",
    x = "SEPI (año-semana)",
    y = "Proporción",
    fill = "Tipo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


#========================================
# 📊 TABLA ANUAL
#========================================

tabla_anual <- confirmados_año %>%
  filter(`anio` >= 2018 & `anio` <= 2025) %>%
  count(`anio`, Evento) %>%
  pivot_wider(
    names_from = Evento,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    Total = rowSums(select(., -`anio`))
  ) %>%
  mutate(
    across(
      -c(`anio`, Total),
      ~ round(.x / Total * 100, 1),
      .names = "prop_{.col}"
    )
  )

#----------------------------------------
# 📈 Gráfico anual
#----------------------------------------
tabla_long <- tabla_anual %>%
  select(anio, starts_with("prop_")) %>%
  pivot_longer(
    cols = -anio,
    names_to = "Evento",
    values_to = "Proporcion"
  ) %>%
  mutate(
    Evento = gsub("prop_", "", Evento),
    Evento = factor(
      Evento,
      levels = c(
        "Hepatitis B",
        "Hepatitis C"
      )
    )
  )

grafico_prop_anio <- ggplot(tabla_long,
                            aes(x = anio, y = Proporcion, fill = Evento)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = unique(tabla_long$anio)) +
  scale_fill_manual(
    values = c(
      "Hepatitis B" = "#EC6C3B",
      "Hepatitis C" = "#525252"
    )
  ) +
  labs(
    title = "Proporción de hepatitis virales confirmadas por año según tipo",
    subtitle = "",
    x = "Año",
    y = "Proporción",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#eaeded"),
    plot.title = element_text(face = "bold", size = 24),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = "right",
    strip.text = element_text(size = 24, face = "bold"),
    axis.line = element_line(linewidth = 1),
    axis.ticks = element_line(linewidth = 1),
    plot.caption = element_text(size = 18)
  )

grafico_prop_anio

#========================================
# 📊 TABLA FINAL CON TOTALES
#========================================

tabla_anual_fa <- tabla_anual %>%
  select(`anio`,`Hepatitis B`, `Hepatitis C`, Total) %>%
  mutate(`anio` = as.character(anio)) %>%
  bind_rows(
    summarise(
      .,
      anio = "Total",
      across(-anio, sum)
    )
  )

tabla_anual_fa
