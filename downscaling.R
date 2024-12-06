# Se borran todas las variables y figuras.
rm(list=ls())
graphics.off()

# =====================================================================
# Codigo para Delta Change y Quantile Mapping / Felipe Garcia
# =====================================================================

# Carga de paquetes:
library(openxlsx)
library(readxl)

# Se fija el directorio de trabajo:
setwd("C:/Users/Usuario/Codigos_R/downscaling")

# Se identifica nombre de la carpeta a utilizar:
carpeta <- "archivos"
ruta_archivos <- paste0(getwd(),"/",carpeta)

# Definición de funciones:

# 1) Funcion para cargar informacion de distintos archivos a dataframes:
cargar_archivos <- function(archivos) {
  # Lista para almacenar los dataframes:
  dataframes <- list()
  
  # Itera sobre cada archivo y cargalo en un dataframe
  for (i in seq_along(archivos)) {
    df_name <- paste0("df_", i) # Crea un nombre unico para cada dataframe
    dataframes[[df_name]] <- read_excel(archivos[i]) # Carga el archivo en lista para almacenar dataframes.
  }
  
  return(dataframes)
}


# Importar data.
# PARA EL CASO DEL EJERCICIO QUE SE HIZO PARA EL DIPLOMADO, SE HAN IMPORTADO
# DATOS DE PRECIPITACIONES DE DISTINTOS GCM. ESTOS DATOS DEBIESEN SER PARA EL CASO
# ESPECIFICO DEL PUNTO EN LA CUENCA DE CAUQUENES QUE ESTAMOS ANALIZANDO (CONFIRMAR).
# LO QUE ME QUEDA COMO TAREA ES HACER EL CODIGO PARA QUE SE CARGUEN LOS GCM EN 
# ARCHIVOS NETCDF QUE TENDRIA GUARDADOS EN UNA CARPETA Y QUE TRABAJE CON LOS DATOS
# DE UN PUNTO ESPECIFICO. POR EL MOMENTO TE RECOMIENDO HACER EL CODIGO TAL CUAL
# ESTA EN LA CLASE Y DESPUES MODIFICARLO.

# Este codigo va a arrojar una lista con la ruta completa, desde C:/User/Usuario ...
archivos <- list.files(ruta_archivos, pattern ="\\.xlsx$", full.names = TRUE)
dataframes <- cargar_archivos(archivos)

# Accede a los dataframes:
pp_ssp245 <- dataframes$df_1
pp_ssp585 <- dataframes$df_2
pp_obs <- dataframes$df_3

#ACA QUEDASTE, REVISA SI LOS DATAFRAMES SE CARGARON BIEN.
# LA FUNCION QUE DEFINISTE ACA EN VDD TE PODRÍA SERVIR PARA TODO TIPO DE ARCHIVOS.
# DEJA ANOTADO ESTO COMO ALGO PARA HACER A FUTURO, OBVIAMENTE HABRÍA QUE MODIFCAR EL CODIGO.

