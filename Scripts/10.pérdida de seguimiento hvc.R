data_C_completa_perdidos <- data_C_completa %>%
  filter (`Perdida de seguimiento`== "SI") # Filtro pacientes HVC con pérdida de seguimiento

data_C_completa_perdidos
