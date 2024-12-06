# Se borran todas las variables y figuras.
rm(list=ls())
graphics.off()

# =====================================================================
# Codigo para Delta Change y Quantile Mapping / Felipe Garcia
# =====================================================================

# Carga de paquetes:
library(openxlsx)

# Se fija el directorio de trabajo:
setwd("C:/Users/Usuario/Codigos_R/downscaling")

# # Se identifica nombre de la carpeta a utilizar:
carpeta <- "archivos"
ruta_archivos <- paste0(getwd(),"/",carpeta)