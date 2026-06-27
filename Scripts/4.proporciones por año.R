# Armo una tabla anual para calcular proporciones

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

# Grafico de proporciones de confirmados por año

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
