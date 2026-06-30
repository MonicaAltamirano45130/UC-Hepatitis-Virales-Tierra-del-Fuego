#====================================================
# TABLA 13: Clasificación de casos de HVC ----
#====================================================
resumen_eventos <- resumen_eventos %>%
  mutate(
    Confirmado = gsub("^[0-9]+\\.", "", Confirmado),
    Confirmado = gsub("\\.", "", Confirmado),
    Confirmado = trimws(Confirmado)
  )

resumen_eventos_gt <- resumen_eventos %>%
  gt() %>%
  cols_label(
    Confirmado = "Clasificación de HVC",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje sobre el total de notificados de HVC"
  ) %>%
  cols_align(align = "center", -Confirmado) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Clasificación de casos de HVC",
    subtitle = "Período 2018–2025"
  )

resumen_eventos_gt

#====================================================
# TABLA 14: Co-infección en HVC ----
#====================================================
co_infección_c <- co_infección_c %>%
  mutate(
    `Co-infección` = gsub("^[0-9]+\\.", "", `Co-infección`),
    `Co-infección` = gsub("\\.", "", `Co-infección`),
    `Co-infección` = trimws(`Co-infección`)
  )

co_infección_c_gt <- co_infección_c %>%
  gt() %>%
  cols_label(
    `Co-infección` = "Co-infección",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje sobre el total de confirmados de HVC"
  ) %>%
  cols_align(align = "center", -`Co-infección`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Co-infección en casos confirmados de HVC",
    subtitle = "Período 2018–2025"
  )

co_infección_c_gt

#====================================================
# TABLA 15: Grupos etarios en HVC ----
#====================================================
confirmados_gedad <- confirmados_gedad %>%
  mutate(
    `Grupo etario diagnóstico` = gsub("^[0-9]+\\.", "", `Grupo etario diagnóstico`),
    `Grupo etario diagnóstico` = gsub("\\.", "", `Grupo etario diagnóstico`),
    `Grupo etario diagnóstico` = trimws(`Grupo etario diagnóstico`)
  )

confirmados_gedad_gt <- confirmados_gedad %>%
  gt() %>%
  cols_label(
    `Grupo etario diagnóstico` = "Grupo etario",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje sobre el total de confirmados de HVC"
  ) %>%
  cols_align(align = "center", -`Grupo etario diagnóstico`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Grupos etarios en HVC",
    subtitle = "Período 2018–2025"
  )

confirmados_gedad_gt

#====================================================
# TABLA 16: Cirrosis en HVC ----
#====================================================
confirmados_cirrosis <- confirmados_cirrosis %>%
  mutate(
    `Cirrosis` = gsub("^[0-9]+\\.", "", `Cirrosis`),
    `Cirrosis` = gsub("\\.", "", `Cirrosis`),
    `Cirrosis` = trimws(`Cirrosis`)
  )

confirmados_cirrosis_gt <- confirmados_cirrosis %>%
  gt() %>%
  cols_label(
    `Cirrosis` = "Cirrosis",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje sobre el total de crónicos con HVC"
  ) %>%
  cols_align(align = "center", -`Cirrosis`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Pacientes con cirrosis en HVC",
    subtitle = "Período 2018–2025"
  )

confirmados_cirrosis_gt

#====================================================
# TABLA 17: Hepatocarcinoma en HVC ----
#====================================================
confirmados_hcc <- confirmados_hcc %>%
  mutate(
    `Hepatocarcinoma` = gsub("^[0-9]+\\.", "", `Hepatocarcinoma`),
    `Hepatocarcinoma` = gsub("\\.", "", `Hepatocarcinoma`),
    `Hepatocarcinoma` = trimws(`Hepatocarcinoma`)
  )

confirmados_hcc_gt <- confirmados_hcc %>%
  gt() %>%
  cols_label(
    `Hepatocarcinoma` = "Hepatocarcinoma",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje sobre el total de confirmados HVC"
  ) %>%
  cols_align(align = "center", -`Hepatocarcinoma`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Pacientes con hepatocarcinoma en HVC",
    subtitle = "Período 2018–2025"
  )

confirmados_hcc_gt

#====================================================
# TABLA 18: Transplante en HVC ----
#====================================================
confirmados_tx <- confirmados_tx %>%
  mutate(
    `Transplante` = gsub("^[0-9]+\\.", "", `Transplante`),
    `Transplante` = gsub("\\.", "", `Transplante`),
    `Transplante` = trimws(`Transplante`)
  )

confirmados_tx_gt <- confirmados_tx %>%
  gt() %>%
  cols_label(
    `Transplante` = "Transplante",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje sobre el total de crónicos con HVC"
  ) %>%
  cols_align(align = "center", -`Transplante`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Pacientes transplantados en HVC",
    subtitle = "Período 2018–2025"
  )

confirmados_tx_gt

#====================================================
# TABLA 19: Fallecidos en HVC ----
#====================================================
confirmados_falle <- confirmados_falle %>%
  mutate(
    `Fallecido` = gsub("^[0-9]+\\.", "", `Fallecido`),
    `Fallecido` = gsub("\\.", "", `Fallecido`),
    `Fallecido` = trimws(`Fallecido`)
  )

confirmados_falle_gt <- confirmados_falle %>%
  gt() %>%
  cols_label(
    `Fallecido` = "Fallecido",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje"
  ) %>%
  cols_align(align = "center", -`Fallecido`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Pacientes fallecidos con HVC",
    subtitle = "Período 2018–2025"
  )

confirmados_falle_gt

#====================================================
# TABLA 20: Pérdida de seguimiento en HVC ----
#====================================================
confirmados_perdidos <- confirmados_perdidos %>%
  mutate(
    `Perdida de seguimiento` = gsub("^[0-9]+\\.", "", `Perdida de seguimiento`),
    `Perdida de seguimiento` = gsub("\\.", "", `Perdida de seguimiento`),
    `Perdida de seguimiento` = trimws(`Perdida de seguimiento`)
  )

confirmados_perdidos_gt <- confirmados_perdidos %>%
  gt() %>%
  cols_label(
    `Perdida de seguimiento` = "Pérdida de seguimiento",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje sobre el total de pacientes con HVC"
  ) %>%
  cols_align(align = "center", -`Perdida de seguimiento`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Pérdida de seguimiento en HVC",
    subtitle = "Período 2018–2025"
  )

confirmados_perdidos_gt

#====================================================
# TABLA 21: Tratamiento en HVC ----
#====================================================
confirmados_tto <- confirmados_tto %>%
  mutate(
    `Tratamiento...26` = gsub("^[0-9]+\\.", "", `Tratamiento...26`),
    `Tratamiento...26` = gsub("\\.", "", `Tratamiento...26`),
    `Tratamiento...26` = trimws(`Tratamiento...26`)
  )

confirmados_tto_gt <- confirmados_tto %>%
  gt() %>%
  cols_label(
    `Tratamiento...26` = "Tratamiento",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje"
  ) %>%
  cols_align(align = "center", -`Tratamiento...26`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Pacientes que recibieron tratamiento con HVC",
    subtitle = "Período 2018–2025"
  )

confirmados_tto_gt

#====================================================
# TABLA 22: RVS en HVC ----
#====================================================
confirmados_rvs <- confirmados_rvs %>%
  mutate(
    `Respuesta viral sostenida` = gsub("^[0-9]+\\.", "", `Respuesta viral sostenida`),
    `Respuesta viral sostenida` = gsub("\\.", "", `Respuesta viral sostenida`),
    `Respuesta viral sostenida` = trimws(`Respuesta viral sostenida`)
  )

confirmados_rvs_gt <- confirmados_rvs %>%
  gt() %>%
  cols_label(
    `Respuesta viral sostenida` = "Respuesta viral sostenida",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje"
  ) %>%
  cols_align(align = "center", -`Respuesta viral sostenida`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Pacientes con RVS en HVC",
    subtitle = "Período 2018–2025"
  )

confirmados_rvs_gt

confirmados_gedad_gt <- confirmados_gedad %>%
  gt() %>%
  cols_label(
    `Grupo etario diagnóstico` = "Grupo etario",
    Casos = "Casos",
    Porcentaje = "Porcentaje"
  ) %>%
  cols_align(align = "center", -`Grupo etario diagnóstico`) %>%
  tab_header(
    title = "Grupos etarios HVC confirmada",
    subtitle = "Período 2018–2025"
  )

confirmados_gedad_gt

confirmados_sexo_gt <- confirmados_sexo %>%
  gt() %>%
  cols_label(`Sexo Legal` = "Sexo", Casos = "Casos", Porcentaje = "Porcentaje") %>%
  cols_align(align = "center", -`Sexo Legal`) %>%
  tab_header(title = "Casos de HVB crónica según sexo")

confirmados_sexo_gt
