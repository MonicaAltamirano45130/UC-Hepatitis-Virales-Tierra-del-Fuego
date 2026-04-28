# IMPORTACIÓN BASES NOMINALES UC-HV

data <- read_excel("~/Unidad Centinela Hepatitis VIrales/UC-Hepatitis-Virales-Tierra-del-Fuego/Data/listado_sisa.xlsx")%>%
 filter ( Evento == "Hepatitis B" | Evento == "Hepatitis C")

Base_HVB <- read_excel("~/Unidad Centinela Hepatitis VIrales/UC-Hepatitis-Virales-Tierra-del-Fuego/Data/Base HVB.xlsx")

Base_HVC <- read_excel("~/Unidad Centinela Hepatitis VIrales/UC-Hepatitis-Virales-Tierra-del-Fuego/Data/Base HVC.xlsx")



