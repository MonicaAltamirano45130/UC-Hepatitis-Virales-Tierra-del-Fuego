
data <- data %>% # Creo la variable fecha_recoleccion
  mutate(
    `Fecha recolección en papel` = na_if(`Fecha recolección en papel`, "*sin dato*"),
    `Fecha recolección en papel` = as.numeric(`Fecha recolección en papel`),
    fecha_recoleccion = as.Date(`Fecha recolección en papel`, origin = "1899-12-30")
  )

data <- data %>%
  mutate(semana = isoweek(`fecha_recoleccion`)) %>% 
  mutate(año = year(`fecha_recoleccion`)) # Creo dos nuevas variables, semana y año a partir de Fecha Apertura

data_b <- data %>%
  filter (Evento == "Hepatitis B")

casos_b_2026 <- data_b %>%
  filter(año == 2026, semana >= 1, semana <= 52) #Filtro los casos notificados desde semana 1 hasta la que quiera reportar del 2026

data_c <- data %>%
  filter (Evento == "Hepatitis C")

casos_c_2026 <- data_c %>%
  filter(año == 2026, semana >= 1, semana <= 52) #Filtro los casos notificados desde semana 1 hasta la que quiera reportar del 2026

write_xlsx(casos_b_2026, "casos_b_2026.xlsx")
write_xlsx(casos_c_2026, "casos_c_2026.xlsx")
