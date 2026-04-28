
# PREPARACIÓN DE LA SESIÓN ===========================

# Cargar librerías

library(dplyr) #transformación de datos
library(lubridate) #trabajo con fechas
library(dlookr) #exploración de datos
library(readr) #lectura de archivos
library(readxl) #lectura de archivos. xlsx
library(writexl) #exportar archivos .xlsx

# 2.1- INSTALAR PAQUETES Y CARGAR LIBRERÍAS (OTRA OPCIÓN)

install.packages("pacman")  #ejecutar si no lo tenés instalado

library(pacman)

pacman::p_load(dplyr,ggplot2,lubridate,
               stringr,readxl, writexl,readr,gt,gtable)


# ⚙️ PROCESAMIENTO DE DATOS ----------------------------------------------------


# 📂 1- IMPORTACIÓN DE DATOS ----------------------------------------------------

#La función a utlizar depende de la extensión de nuestro archivo

# Archivo separados por comas (.csv)

data <- read_xlsx("Data/Base HVC.xlsx")

# 🔎 2- EXPLORACIÓN DEL CONJUNTO DE DATOS ================================================

str(data)                # Estructura del dataset
glimpse(data)

colnames(data)           # Nombres de las columnas


###########################
## Detección de valores NA
###########################

## Usamos dlookr para detección de valores NA en todas las variables

find_na(data, rate = T)

## Graficamos

plot_na_pareto(data)


# Verificación de valores faltantes

#¿Cuántos valores faltantes hay en las varaibles del data frame?
find_na(data, rate = TRUE)

# ¿Cuántos valores faltantes hay en una varible determinada?
sum(is.na(data$"Nro Doc")) 
