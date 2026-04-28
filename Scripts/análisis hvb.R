# ACTIVACIÓN DE LIBRERÍAS A UTILIZAR 

library(dplyr) #transformación de datos
library(lubridate) #trabajo con fechas
library(dlookr) #exploración de datos
library(readr) #lectura de archivos
library(readxl) #lectura de archivos. xlsx
library(writexl) #exportar archivos .xlsx
library(gt) # libreria para tablas 
library(gtable) #libreria para tablas 
library (janitor)#librería para addorn totals

# IMPORTACIÓN BASES NOMINALES UC-HV

data <- read_excel("~/Unidad Centinela Hepatitis VIrales/UC-Hepatitis-Virales-Tierra-del-Fuego/Data/listado_sisa.xlsx")%>%
  filter (Evento == "Hepatitis B")

Base_HVB <- read_excel("~/Unidad Centinela Hepatitis VIrales/UC-Hepatitis-Virales-Tierra-del-Fuego/Data/Base HVB.xlsx")

data <- data %>% # Selecciono columnas 
    select( `Nro Doc`,`Grupo etario diagnóstico`,`Grupo de evento`, `Clasificación manual del caso` )

data_B_completa <- left_join(data, Base_HVB, by = "Nro Doc") # Uno las bases

data_B_completa <- data_B_completa %>% # Quito filas NA
  filter(!is.na(`Clasificación`))

resumen_eventos <- data_B_completa %>% # Resumo clasificaciones 
  group_by(`Clasificación`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

resumen_eventos

confirmados_b <- data_B_completa %>%
  filter (Clasificación== "Aguda" |Clasificación== "Crónica"|Clasificación==  "Resuelta" |Clasificación==  "aguda y resuelta"|Clasificación==  "Sin datos" )

count(confirmados_b)

co_infección_b <- confirmados_b %>% # Resumo co-infección
  group_by(`Co-infección`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

co_infección_b 

aguda_resuelta <- confirmados_b %>% # Resumo agudas por gedad
  filter (`Clasificación` == "Aguda" |Clasificación == "aguda y resuelta" )

count(aguda_resuelta)

aguda_resuelta_gedad <- aguda_resuelta %>% 
  group_by(`Grupo etario diagnóstico`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

aguda_resuelta_gedad 

cronica <- confirmados_b %>% # Filtro HVB crónica
  filter (`Clasificación` == "Crónica")

count(cronica)

cronica_gedad <- cronica %>% # Resumo crónicas por gedad
  group_by(`Grupo etario diagnóstico`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

cronica_gedad

cronica_cirrosis <- cronica %>% # Cirrosis en crónicas
  group_by(`Cirrosis`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total 

cronica_cirrosis

cronica_hcc <- cronica %>% # HCC en crónicas
  group_by(`Hepatocarcinoma`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total 


cronica_hcc

cronica_tx <- cronica %>% # Transplante en crónicas
  group_by(`Transplante`)%>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

cronica_tx

cronica_falle <- cronica %>% # Fallecidos en crónicas
  group_by(`Fallecido`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total 

cronica_falle

cronica_perdidos <- cronica %>% # Pérdida de seguimiento en crónicas
  group_by(`Pérdida de seguimiento`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total 

cronica_perdidos

cronica_tto <- cronica %>% # Tratamientos en crónicas
  group_by(`Tratamiento...26`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100, 1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

cronica_tto

perdidos <- confirmados_b %>% # Pérdida de seguimiento en crónicas
  group_by(`Pérdida de seguimiento`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total 

perdidos

sin_datos <- confirmados_b %>% # Filtro Sin datos
  filter (`Clasificación` == "Sin datos")

count(sin_datos)

sin_datos <- sin_datos %>% # Notificadores de casos sin datos
  group_by(`Grupo de evento.x`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total 

sin_datos
#================================================================================================
#===============================================================================================

#️ 📁️️️️  4- EXPORTAR BASE DE DATOS ================================================

#La función a utilizar, depende del formato en el que queramos guardar la base
# de datos ya procesada. El archivo procesado, se guardará en la carpeta que indiquemos
# en la ruta. 

# Formato excel
write_xlsx(data_B_completa, "Salidas/data_B_completa.xlsx")

# Formato .csv 
write_csv(data_B_completa, "Salidas/data_B_completa.csv")

#========================================================================


