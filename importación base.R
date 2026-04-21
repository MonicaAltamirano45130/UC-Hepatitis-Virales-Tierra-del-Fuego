# IMPORTACIÓN BASES NOMINALES UC-HV

data <- read_excel("~/Unidad Centinela Hepatitis VIrales/UC-Hepatitis-Virales-Tierra-del-Fuego/Data/listado_sisa.xlsx")

Base_HVB <- read_excel("~/Unidad Centinela Hepatitis VIrales/UC-Hepatitis-Virales-Tierra-del-Fuego/Data/Base HVB.xlsx")

Base_HVC <- read_excel("~/Unidad Centinela Hepatitis VIrales/UC-Hepatitis-Virales-Tierra-del-Fuego/Data/Base HVC.xlsx")

# ===== PARÁMETROS TEMPORALES PARA EL ANÁLISIS ======

#Defino parámetros temporales de análisis (establecidos en el plan)

ANIO_MINIMO <- 2019

SEMANA_MINIMA <- 1

ANIO_MAXIMO <- 2025

SEMANA_MAXIMA <- 52