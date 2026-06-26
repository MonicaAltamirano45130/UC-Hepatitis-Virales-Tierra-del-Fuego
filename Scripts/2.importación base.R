#========================================
# 📥 IMPORTACIÓN DE BASES
#========================================

# Base nominal UC-HV
data <- read_excel("Data/listado_sisa.xlsx")

# Base HVB
Base_HVB <- read_excel("Data/Base HVB.xlsx")

# Base HVC
Base_HVC <- read_excel("Data/Base HVC.xlsx")

# Base población
poblacion <- read_excel("Data/población.xlsx")


#========================================
# 🧹 LIMPIEZA Y TRANSFORMACIÓN
#========================================

#----- DATA PRINCIPAL -----
data <- data %>%
  mutate(
    # Limpieza de fecha
    `Fecha recolección en papel` = na_if(`Fecha recolección en papel`, "*sin dato*"),
    `Fecha recolección en papel` = as.numeric(`Fecha recolección en papel`),
    fecha_recoleccion = as.Date(`Fecha recolección en papel`, origin = "1899-12-30"),
    
    # Variables temporales
    semana_papel = isoweek(fecha_recoleccion),
    año_papel = year(fecha_recoleccion),
    semana_apertura = isoweek(`Fecha Apertura`),
    año_apertura = year(`Fecha Apertura`),
    
    # SEPI
    SEPI_papel = paste0(año_papel, "-", sprintf("%02d", semana_papel))
  ) %>%
  filter(
    año_apertura >= 2018,
    between(año_papel, 2018, 2025)
  ) %>%
  select(
    `Nro Doc`,
    `Apellido Ciudadano`,
    `Nombre Ciudadano`,
    `Grupo etario diagnóstico`,
    `Sexo Legal`,
    `Evento`,
    `Grupo de evento`,
    `Clasificación manual del caso`,
    semana_papel,
    año_papel,
    SEPI_papel,
    `Fecha Apertura`,
    fecha_recoleccion,
    `Localidad Residencia`
  )

# Corrección de texto
data$`Grupo etario diagnóstico` <- gsub("a¿os", "años", data$`Grupo etario diagnóstico`)


#----- BASE HVB -----
Base_HVB <- Base_HVB %>%
  select(
    `Nro Doc`,
    `Clasificación`,
    `Co-infección`,
    `Tratamiento...26`,
    `Transplante`,
    `Hepatocarcinoma`,
    `Cirrosis`,
    `Pérdida de seguimiento`,
    `Fallecido`
  )


#----- BASE HVC -----
Base_HVC <- Base_HVC %>%
  select(
    `Nro Doc`,
    `Confirmado`,
    `Co-infección`,
    `Tratamiento...26`,
    `Transplante`,
    `Hepatocarcinoma`,
    `Cirrosis`,
    `Respuesta viral sostenida`,
    `Perdida de seguimiento`,
    `Fallecido`
  )

#========================================
# 📊 FILTRO Y UNIÓN BASE HVB
#========================================

data_b <- data %>%
  filter(Evento == "Hepatitis B")

data_B_completa <- left_join(data_b, Base_HVB, by = "Nro Doc")

data_B_completa <- data_B_completa %>%
  filter(!is.na(`Clasificación`))


#========================================
# 📊 FILTRO Y UNIÓN BASE HVC
#========================================

data_c <- data %>%
  filter(Evento == "Hepatitis C")

data_C_completa <- left_join(data_c, Base_HVC, by = "Nro Doc")

data_C_completa <- data_C_completa %>%
  filter(!is.na(`Confirmado`))

