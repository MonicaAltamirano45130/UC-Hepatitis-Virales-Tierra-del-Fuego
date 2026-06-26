#====================================================
# TABLA 1: Totales anuales de HV ----
#====================================================
tabla_anual_fa_gt <- tabla_anual_fa %>%
  gt() %>%
  cols_label(
    anio = "Año",
    `Hepatitis B` = "Casos B",
    `Hepatitis C` = "Casos C",
    Total = "Total HV"
  ) %>%
  cols_align(align = "center", -anio) %>%
  tab_header(
    title = "Totales anuales de hepatitis virales",
    subtitle = "Período 2018–2025"
  )
tabla_anual_fa_gt

#====================================================
# TABLA 2: Casos y tasas de HV ----
#====================================================
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
tabla_final_gt

#====================================================
# TABLA 3: Clasificación HVB ----
#====================================================
resumen_eventos_b <- resumen_eventos_b %>%
  mutate(
    Clasificación = gsub("^[0-9]+\\.", "", Clasificación),
    Clasificación = gsub("\\.", "", Clasificación),
    Clasificación = trimws(Clasificación)
  )

resumen_eventos_b_gt <- resumen_eventos_b %>%
  gt() %>%
  cols_label(
    Clasificación = "Clasificación de HBV",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje sobre el total de HVB"
  ) %>%
  cols_align(align = "center", -Clasificación) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Clasificación de casos de HVB",
    subtitle = "Período 2018–2025"
  )
resumen_eventos_b_gt

#====================================================
# TABLA 4: Co-infección HVB ----
#====================================================
co_infección_b <- co_infección_b %>%
  mutate(
    `Co-infección` = gsub("^[0-9]+\\.", "", `Co-infección`),
    `Co-infección` = gsub("\\.", "", `Co-infección`),
    `Co-infección` = trimws(`Co-infección`)
  )

co_infección_b_gt <- co_infección_b %>%
  gt() %>%
  cols_label(
    `Co-infección` = "Co-infección",
    Casos = "Número de casos",
    Porcentaje = "Porcentaje"
  ) %>%
  cols_align(align = "center", -`Co-infección`) %>%
  fmt_number(columns = Porcentaje, decimals = 1) %>%
  tab_header(
    title = "Co-infección en HVB",
    subtitle = "Período 2018–2025"
  )
co_infección_b_gt

#====================================================
# TABLA 5: Grupos etarios HVB aguda ----
#====================================================
aguda_resuelta_gedad <- aguda_resuelta_gedad %>%
  mutate(
    `Grupo etario diagnóstico` = trimws(`Grupo etario diagnóstico`)
  )

aguda_resuelta_gedad_gt <- aguda_resuelta_gedad %>%
  gt() %>%
  cols_label(
    `Grupo etario diagnóstico` = "Grupo etario",
    Casos = "Casos",
    Porcentaje = "Porcentaje"
  ) %>%
  cols_align(align = "center", -`Grupo etario diagnóstico`) %>%
  tab_header(
    title = "Grupos etarios HVB aguda",
    subtitle = "Período 2018–2025"
  )
aguda_resuelta_gedad_gt

#====================================================
# TABLA 6: Grupos etarios HVB crónica ----
#====================================================
cronica_gedad <- cronica_gedad %>%
  mutate(
    `Grupo etario diagnóstico` = trimws(`Grupo etario diagnóstico`)
  )

cronica_gedad_gt <- cronica_gedad %>%
  gt() %>%
  cols_label(
    `Grupo etario diagnóstico` = "Grupo etario",
    Casos = "Casos",
    Porcentaje = "Porcentaje"
  ) %>%
  cols_align(align = "center", -`Grupo etario diagnóstico`) %>%
  tab_header(
    title = "Grupos etarios HVB crónica",
    subtitle = "Período 2018–2025"
  )
cronica_gedad_gt

#====================================================
# TABLA 7: Cirrosis HVB ----
#====================================================
cronica_cirrosis_gt <- cronica_cirrosis %>%
  gt() %>%
  cols_label(Cirrosis = "Cirrosis", Casos = "Casos", Porcentaje = "Porcentaje") %>%
  cols_align(align = "center", -Cirrosis) %>%
  tab_header(title = "Cirrosis en HVB")

cronica_cirrosis_gt

#====================================================
# TABLA 8: Hepatocarcinoma HVB ----
#====================================================
cronica_hcc_gt <- cronica_hcc %>%
  gt() %>%
  cols_label(Hepatocarcinoma = "HCC", Casos = "Casos", Porcentaje = "Porcentaje") %>%
  cols_align(align = "center", -Hepatocarcinoma) %>%
  tab_header(title = "Hepatocarcinoma en HVB")

cronica_hcc_gt

#====================================================
# TABLA 9: Transplante HVB ----
#====================================================
cronica_tx_gt <- cronica_tx %>%
  gt() %>%
  cols_label(Transplante = "Transplante", Casos = "Casos", Porcentaje = "Porcentaje") %>%
  cols_align(align = "center", -Transplante) %>%
  tab_header(title = "Transplante en HVB")

cronica_tx_gt

#====================================================
# TABLA 10: Fallecidos HVB ----
#====================================================
cronica_falle_gt <- cronica_falle %>%
  gt() %>%
  cols_label(Fallecido = "Fallecido", Casos = "Casos", Porcentaje = "Porcentaje") %>%
  cols_align(align = "center", -Fallecido) %>%
  tab_header(title = "Fallecidos en HVB")

cronica_falle_gt

#====================================================
# TABLA 11: Pérdida seguimiento HVB ----
#====================================================
cronica_perdidos_gt <- cronica_perdidos %>%
  gt() %>%
  cols_label(`Pérdida de seguimiento` = "Seguimiento", Casos = "Casos", Porcentaje = "Porcentaje") %>%
  cols_align(align = "center", -`Pérdida de seguimiento`) %>%
  tab_header(title = "Pérdida de seguimiento HVB")

cronica_perdidos_gt

#====================================================
# TABLA 12: Tratamiento HVB ----
#====================================================
cronica_tto_gt <- cronica_tto %>%
  gt() %>%
  cols_label(`Tratamiento...26` = "Tratamiento", Casos = "Casos", Porcentaje = "Porcentaje") %>%
  cols_align(align = "center", -`Tratamiento...26`) %>%
  tab_header(title = "Tratamiento en HVB")

cronica_tto_gt