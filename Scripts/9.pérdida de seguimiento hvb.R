
data_B_completa_perdidos <- data_B_completa %>%
  filter (`Pérdida de seguimiento`== "si" |`Pérdida de seguimiento`== "SI") # Filtro pacientes HVB con pérdida de seguimiento

data_B_completa_perdidos
