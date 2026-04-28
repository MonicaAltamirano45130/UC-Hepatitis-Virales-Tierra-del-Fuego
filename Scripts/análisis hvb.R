
# Análisis HV  SNVS ======================================================

# Análisis HVB ======================================================
data_B <- data %>% filter(Evento == "Hepatitis B") # Filtramos el evento Hepatitis B

data_columnas_interes <- data_B %>% # Selecciono columnas de interés
  select(Evento,`Clasificación manual del caso`,`Sexo Legal`,`Grupo etario diagnóstico`, `Nro Doc`,`Fecha Apertura`, `Edad Diagnostico`, `Sexo Legal`)

resumen_eventos <- data_B %>% # Resumo y agrupo las clasificaciones manuales
  group_by(`Clasificación manual del caso`) %>%  #agrupa las filas por cada valor distinto en la variable  Clasificación manual del caso
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (Clasificación manual) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_eventos_ordenado <- resumen_eventos %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# Análisis HV Hepatología ======================================================

data_columnas_interes_B <- Base_HVB %>% # Selecciono columnas de interés
  select(`Nro Doc`,`Edad Diagnostico`,`Sexo Legal`,Clasificación,Transplante,Hepatocarcinoma,Cirrosis,`Pérdida de seguimiento`,`Co-infección`)

# Unión de bases de SNVS y hepatología filtradas según DNI ======================================================
data_B_completa <- left_join(data_columnas_interes, data_columnas_interes_B, by = "Nro Doc") 

colnames(data_B_completa) #Observo los nombres de las columnas de la base completa

unique (data_B_completa$Clasificación)

data_B_completa <- data_B_completa %>%
  filter (Clasificación== "Aguda" |Clasificación== "Crónica"|Clasificación==  "Resuelta" |Clasificación==  "aguda y resuelta")

# CALCULO DE SEMANA EPIDEMIOLÓGICA PARA EL ANÁLISIS

# La función epiweek() de la librería lubridate, nos permite calcular la
# SE a partir de una fecha determinada.

data_B_completa <- data_B_completa %>%
  mutate(SEPI = epiweek(`Fecha Apertura`))

# ===== PARÁMETROS TEMPORALES PARA EL ANÁLISIS ======

#Defino parámetros temporales de análisis (establecidos en el plan)

ANIO_MINIMO <- 2019

SEMANA_MINIMA <- 1

ANIO_MAXIMO <- 2025

SEMANA_MAXIMA <- 52

# 📚AGRUPAR Y CONTAR DATOS =======================================

resumen_eventos_completos <- data_B_completa %>% 
  group_by(`Clasificación`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_eventos_completos <- resumen_eventos_completos %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 
 
          
resumen_eventos_completos   

# 📚AGRUPAR Y CONTAR DATOS =======================================

resumen_gedad <- data_B_completa %>% 
  group_by(`Grupo etario diagnóstico`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() %>% # agregamos totales # desagrupar los datos para evitar afectaciones posteriores
  adorn_totals(where = "row")  

# 📚AGRUPAR Y CONTAR DATOS =======================================

resumen_sexo <- data_B_completa %>% 
  group_by(`Sexo Legal.y`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_sexo <- resumen_sexo %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# 📚AGRUPAR Y CONTAR DATOS =======================================
resumen_coinfeccion <- data_B_completa %>% 
  group_by(`Co-infección`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_coinfeccion <- resumen_coinfeccion %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_transplante <- data_B_completa %>% 
  group_by(`Transplante`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_transplante <- resumen_transplante %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

1# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_hepatocarcinoma <- data_B_completa %>% 
  group_by(`Hepatocarcinoma`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_hepatocarcinoma <- resumen_hepatocarcinoma %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_cirrosis <- data_B_completa %>% 
  group_by(`Cirrosis`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_cirrosis <- resumen_cirrosis %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_perdida <- data_B_completa %>% 
  group_by(`Pérdida de seguimiento`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_perdida <- resumen_perdida %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

#️ 📁️️️️  4- EXPORTAR BASE DE DATOS ================================================

#La función a utilizar, depende del formato en el que queramos guardar la base
# de datos ya procesada. El archivo procesado, se guardará en la carpeta que indiquemos
# en la ruta. 

# Formato excel
write_xlsx(data_B_completa, "Salidas/data_B_completa.xlsx")

# Formato .csv 
write_csv(data_B_completa, "Salidas/data_B_completa.csv")

#========================================================================


