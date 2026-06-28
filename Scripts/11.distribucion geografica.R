colnames (data)

localidad <- data %>%
  group_by(`Localidad Residencia`) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Casos / sum(Casos)) * 100, 1)) %>%
  arrange(desc(Porcentaje)) %>%
  adorn_totals(where = "row")

localidad_gt <- localidad%>%
  gt() %>%
  cols_label(
    `Localidad Residencia` = "Localidad",
    Casos = "Casos notificados",
    Porcentaje = "% Notificados por localidad"
  ) %>%
  cols_align(align = "center", -`Localidad Residencia`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Notificados de HV por localidad",
    subtitle = "Período 2018–2025"
  )
localidad_gt
