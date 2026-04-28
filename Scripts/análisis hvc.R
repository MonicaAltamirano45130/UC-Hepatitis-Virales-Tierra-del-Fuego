
# Análisis HV  SNVS ======================================================

# Análisis HVC ======================================================
data_C <- data %>% filter(Evento == "Hepatitis C") # Filtramos el evento Hepatitis B

data_columnas_interes <- data_C %>% # Selecciono columnas de interés
  select(Evento,`Clasificación manual del caso`,`Sexo Legal`,`Grupo etario diagnóstico`, `Nro Doc`,`Fecha Apertura`)

resumen_eventos <- data_C %>% # Resumo y agrupo las clasificaciones manuales
  group_by(`Clasificación manual del caso`) %>%  #agrupa las filas por cada valor distinto en la variable  Clasificación manual del caso
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (Clasificación manual) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_eventos_ordenado_c <- resumen_eventos %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# Análisis HV Hepatología ======================================================

data_columnas_interes_C <- Base_HVC %>% # Selecciono columnas de interés
  select(`Nro Doc`,Confirmado,Transplante,Hepatocarcinoma,Cirrosis,`Respuesta viral sostenida`,`Perdida de seguimiento`,`Co-infección`)

# Unión de bases de SNVS y hepatología filtradas según DNI ======================================================
data_C_completa <- left_join(data_columnas_interes, data_columnas_interes_C, by = "Nro Doc") 

colnames(data_C_completa) #Observo los nombres de las columnas de la base completa

unique (data_C_completa$Confirmado)

data_C_completa <- data_C_completa %>%
  filter (Confirmado== "SI" )


# CALCULO DE SEMANA EPIDEMIOLÓGICA PARA EL ANÁLISIS

# La función epiweek() de la librería lubridate, nos permite calcular la
# SE a partir de una fecha determinada.

data_C_completa <- data_C_completa %>%
  mutate(SEPI = epiweek(`Fecha Apertura`))

# 📚AGRUPAR Y CONTAR DATOS =======================================

resumen_eventos_completos_c <- data_C_completa %>% 
  group_by(`Confirmado`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_eventos_completos_c <- resumen_eventos_completos_c %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# 📚AGRUPAR Y CONTAR DATOS =======================================

resumen_gedad_c <- data_C_completa %>% 
  group_by(`Grupo etario diagnóstico`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() %>% 
  adorn_totals(where = "row")  # agregamos totales # desagrupar los datos para evitar afectaciones posteriores

# 📚AGRUPAR Y CONTAR DATOS =======================================

resumen_sexo_c <- data_C_completa %>% 
  group_by(`Sexo Legal`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_sexo_c <- resumen_sexo_c %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# 📚AGRUPAR Y CONTAR DATOS =======================================
resumen_coinfeccion_c <- data_C_completa %>% 
  group_by(`Co-infección`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_coinfeccion_c <- resumen_coinfeccion_c %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_transplante_c <- data_C_completa %>% 
  group_by(`Transplante`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_transplante_c <- resumen_transplante_c %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_hepatocarcinoma_c <- data_C_completa %>% 
  group_by(`Hepatocarcinoma`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_hepatocarcinoma_c <- resumen_hepatocarcinoma_c %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_cirrosis_c <- data_C_completa %>% 
  group_by(`Cirrosis`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_cirrosis_c <- resumen_cirrosis_c %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_rvs_c <- data_C_completa %>% 
  group_by(`Cirrosis`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_rvs_c <- resumen_rvs_c %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

# GROUP_BY + SUMMARISE ================================================
# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_perdida_c <- data_C_completa %>% 
  group_by(`Perdida de seguimiento`) %>%  #agrupa las filas por cada valor distinto en la variable  DEPARTAMENTO_RESIDENCIA
  summarise(Casos = n()) %>% #cuenta cuántas filas hay en cada grupo (departamento) y guarda el conteo en una columna llamada casos
  ungroup() # desagrupar los datos para evitar afectaciones posteriores

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_perdida_c <- resumen_perdida_c %>% 
  arrange(desc(Casos))%>% 
  adorn_totals(where = "row")  # agregamos totales 

#️ 📁️️️️  4- EXPORTAR BASE DE DATOS ================================================

#La función a utilizar, depende del formato en el que queramos guardar la base
# de datos ya procesada. El archivo procesado, se guardará en la carpeta que indiquemos
# en la ruta. 

# Formato excel
write_xlsx(data_C_completa, "Salidas/data_B_completa.xlsx")

# Formato .csv 
write_csv(data_C_completa, "Salidas/data_B_completa.csv")

#========================================================================


