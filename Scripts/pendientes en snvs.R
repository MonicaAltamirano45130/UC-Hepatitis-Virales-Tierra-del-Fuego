
tabla_NA <- data_B_completa %>%
  filter(is.na(`Clasificación`))

library(writexl)

write_xlsx(tabla_NA, "tabla_NA.xlsx")



