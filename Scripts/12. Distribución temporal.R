#============================================================================
# SCRIPT 5 - OBJETIVO 1 - DISTRIBUCIÓN TEMPORAL 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


# 1- BASE DE TRABAJO ------------------------------------------------------

# Para este objetivo se utiliza la base analítica que se generó 
# en el SCRIPTS 4

data_obj1 <- data_B_completa


# CASOS POR SE Y ANIO -----------------------------------------------------
# Agrupo casos por año y semana epidemiológica

casos_semana_anio <- data_obj1 %>% 
  group_by(SEPI_papel,Clasificación) %>%
  summarise(CASOS = n()) %>%
  ungroup() %>%
  arrange(SEPI_papel) 

# Paso datos a formato ancho (wider) para hacer curva interactiva

casos_semana_anio <- casos_semana_anio %>% 
  pivot_wider(names_from = Clasificación,
              values_from = CASOS,
              values_fill = 0) 


# CURVA INTERACTIVA CASOS DE IRAG E IRAGE ---------------------------------

curva_interactiva <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(
    text = "Distribución temporal de HV")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = casos_semana_anio$SEPI_papel, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_add_series(
    data = casos_semana_anio$`Aguda`,
    name = "Hepatitis B Aguda",
    color = "#252C61") %>%
  hc_add_series(
    data = casos_semana_anio$`Crónica`,
    name = "Hepatitis B Crónica",
    color = "#7EC8E6")

curva_interactiva_b

#============================================================================
# SCRIPT 5 - OBJETIVO 1 - DISTRIBUCIÓN TEMPORAL 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


# 1- BASE DE TRABAJO ------------------------------------------------------

# Para este objetivo se utiliza la base analítica que se generó 
# en el SCRIPTS 4

data_obj1 <- data_C_completa


# CASOS POR SE Y ANIO -----------------------------------------------------
# Agrupo casos por año y semana epidemiológica

casos_semana_anio <- data_obj1 %>% 
  group_by(SEPI_papel,Confirmado) %>%
  summarise(CASOS = n()) %>%
  ungroup() %>%
  arrange(SEPI_papel) 

# Paso datos a formato ancho (wider) para hacer curva interactiva

casos_semana_anio <- casos_semana_anio %>% 
  pivot_wider(names_from = Confirmado,
              values_from = CASOS,
              values_fill = 0) 


# CURVA INTERACTIVA CASOS DE IRAG E IRAGE ---------------------------------

curva_interactiva <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(
    text = "Distribución temporal de HV")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = casos_semana_anio$SEPI_papel, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_add_series(
    data = casos_semana_anio$`SI`,
    name = "Hepatitis C",
    color = "#252C61") %>%
  hc_add_series(
    data = casos_semana_anio$`Crónica`,
    name = "Hepatitis B Crónica",
    color = "#7EC8E6")

curva_interactiva_c

