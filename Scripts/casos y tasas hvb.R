# PROCESAMIENTO DE LOS DATOS

#Filtro evento de interés y año en estudio

base <- data_B_completa %>% filter (Clasificación != "descartado") 

# ----------------------------------------------------
# 🧮 FRECUENCIA ABSOLUTA Y RELATIVA
# ----------------------------------------------------

# Frecuencia absoluta

casos_crónicos <- sum(base$Clasificación == "Crónica", na.rm = TRUE)

casos_agudos <-  sum(base$Clasificación == "Aguda", na.rm = TRUE)

casos_resueltos <-  sum(base$Clasificación == "Resuelta", na.rm = TRUE)

casos_agudos_resueltos <-  sum(base$Clasificación == "aguda y resuelta", na.rm = TRUE)

casos_totales <- sum(casos_crónicos+casos_agudos+casos_resueltos+casos_agudos_resueltos)

# Frecuencia relativa

proporcion_crónicos <- round((casos_crónicos/casos_totales)*100,1)

proporcion_agudos <- round((casos_agudos/casos_totales)*100,1)

# -------------------------------------------------------
# 🔢 MEDIDAS DE TENDENCIA CENTRAL, DISPERSIÓN Y POSICIÓN 
# -------------------------------------------------------

# Medidas de tendencia central

media <- mean(base$`Edad Diagnostico.y`,na.rm = TRUE)

media_redondeada <- round(mean(base$`Edad Diagnostico.y`,na.rm = TRUE),1)

mediana <- median(base$`Edad Diagnostico.y`,na.rm = TRUE)

# Medidas de dispersión

desvio <-sd(base$`Edad Diagnostico.y`,na.rm = TRUE)

varianza <-var(base$`Edad Diagnostico.y`,na.rm = TRUE)


#Medidas de posición

Q1 <- quantile(base$`Edad Diagnostico.y`, 0.25, na.rm = TRUE)

Q2 <- quantile(base$`Edad Diagnostico.y`, 0.50, na.rm = TRUE)

Q3 <- quantile(base$`Edad Diagnostico.y`, 0.75, na.rm = TRUE)

rango_intercuartilico <- IQR(base$`Edad Diagnostico.y`, na.rm = TRUE)

minimo <- min(base$`Edad Diagnostico.y`, na.rm = TRUE)

maximo <- max(base$`Edad Diagnostico.y`, na.rm = TRUE)


# =====================================================
# GRAFICO GGPLOT2: CURVA EPIDEMIOLÓGICA 
# =====================================================

# Preparación del data frame

#Agrupo casos notificados por semana epidemiológica

curva_epidemiologica_casos <- base %>% group_by(Clasificación,SEPI) %>%
  summarise(n = n()) %>%
  ungroup()

#Completo tabla con las SE donde no hubo casos notificados. El número de casos se completa con 0

curva_epidemiologica_casos <- curva_epidemiologica_casos %>% 
  complete(SEPI = 1:53,
           fill = list (n= 0))

# Creo la variable SE para utilizar como etiqueta del eje x.Se 
# normaliza la escritura para que todas las SE estén compuestas por 2 dígitos

curva_epidemiologica_casos <- curva_epidemiologica_casos %>% mutate(SEPI = str_pad(SEPI, #variable a normalizar
                                                                                 width = 2, #cantidad de dígitos
                                                                                 side = "left", #posición del número que se utilizará para "completar"
                                                                                 pad = "0")) #número que se utilizará para "completar"

# Gráfico- Casos de IRAG e IRAGE notificados por SE en ggplot2

colores <- c("#7fc97f","#beaed4") 

grafico_curva_casos <- ggplot(curva_epidemiologica_casos, aes(y= n, x = SEPI, fill= Clasificación)) +
  geom_col () +
  labs (title = "",
        x = "Semana epidemiológica",
        y = "Casos",
        caption = "Fuente: elaboración propia a partir de datos del SNVS 2.0.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  scale_fill_manual(values = colores,
                    name = "",
                    labels = c("Crónica","Aguda"),
                    na.translate = FALSE) +
  theme(legend.position = "bottom")


grafico_curva_casos


# =====================================================
# GRAFICO HIGHCHARTER: CURVA EPIDEMIOLÓGICA 
# =====================================================

curva_wider <- curva_epidemiologica_casos %>% pivot_wider(names_from = Clasificación,
                                                          values_from = n)

curva_interactiva <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = curva_wider$SEPI, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = curva_wider$`Crónica`,
    name = "Crónica",
    color = "#7fc97f") %>%
  hc_add_series(
    data = curva_wider$`Aguda`,
    name = "Aguda",
    color = "#beaed4") 

curva_interactiva

