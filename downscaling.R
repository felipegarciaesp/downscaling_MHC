# Se borran todas las variables y figuras.
rm(list=ls())
graphics.off()

# =====================================================================
# Codigo para Delta Change y Quantile Mapping / Felipe Garcia
# =====================================================================

# Carga de paquetes:
library(openxlsx)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

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

# 2) Funcion para convertir a formato fecha los datos de una columna en especifico:
convertir_a_fecha <- function(df, columna, start_date, end_date, formato = "%Y-%m-%d") {
  # Creamos un vector de fechas:
  fechas <- seq(from = as.Date(start_date), to = as.Date(end_date), 
                       by = 'month')
  # Reemplazamos este vector en la columna correspondiente
  df[[columna]] <- fechas
  return(df)
}

# 3) Funcion para filtrar el df ingresado a las fechas que nos interesan:
filtrado <- function(df, column, start_date, end_date) {
  df <- df[df[[column]] >= start_date & df[[column]] <= end_date, ]
  return(df)
}

# 4) Funcion para obtener los promedios mensuales de un dataframe:
# Esta funcion la podrías acomodar para que calcule no solo mean, sino que sum,
# o alguna otra funcion.
mean_mon <- function(df,column) {
  df_mean_mon <- df %>%
    group_by(month(df[[column]])) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    rename(Mes = `month(df[[column]])`) # Nos aseguramos que la primera columna
                                        # tenga como nombre 'Mes'.
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

# Se va a eliminar la primera columna de los df de proyecciones ssp:
pp_ssp245 <- pp_ssp245[,-1]
pp_ssp585 <- pp_ssp585[,-1]

# Se va a cambiar formato de fechas de pp_obs, para trabajar mejor:
pp_obs$Fechas <- as.Date(pp_obs$Fechas)

# Los df de los escenarios ssp no tienen reconocidas las fechas como formato fechas
# Se va a solucionar esto:
start_date <- '1850-01-01'
end_date <- '2100-12-01'
pp_ssp245 <- convertir_a_fecha(pp_ssp245, 'V1', start_date, end_date)
pp_ssp585 <- convertir_a_fecha(pp_ssp585, 'V1', start_date, end_date)

# LA FUNCION cargar_archivos TE PODRÍA SERVIR PARA TODO TIPO DE ARCHIVOS.
# DEJA ANOTADO ESTO COMO ALGO PARA HACER A FUTURO, OBVIAMENTE HABRÍA QUE MODIFICAR 
# EL CODIGO.

# Delta Change Precipitacion Mensual

# Tenemos ya los df cargados con informacion, pero solo vamos a ocupar los 
# correspondientes al periodo historico (1979-2014) y futuro (2065-2100).
# Vamos a filtrar estas fechas:

# Definimos las fechas de inicio y fin de los periodos historico y futuro.
start_hist <- '1979-01-01'
end_hist <- '2014-12-01'

start_fut <- '2065-01-01'
end_fut <- '2100-12-01'

# Ocupamos la funcion filtrado para filtrar para las fechas de interes:
pp_obs <- filtrado(pp_obs, 'Fechas', start_hist, end_hist)
pp_ssp245_hist <- filtrado(pp_ssp245, 'V1', start_hist, end_hist)
pp_ssp585_hist <- filtrado(pp_ssp585, 'V1', start_hist, end_hist)
pp_ssp245_fut <- filtrado(pp_ssp245, 'V1', start_fut, end_fut)
pp_ssp585_fut <- filtrado(pp_ssp585, 'V1', start_fut, end_fut)

# Obtenemos las precipitaciones medias mensuales de las serie observada y para
# la serie historica de ambas proyecciones ssp:
pp_obs_prom_mens <- mean_mon(pp_obs, 'Fechas')
pp_ssp245_prom_mens_hist <- mean_mon(pp_ssp245_hist, 'V1')
pp_ssp585_prom_mens_hist <- mean_mon(pp_ssp585_hist, 'V1')

# Graficos:

# Convierte el dataframe de los GCM a formato largo
pp_ssp585_long <- pivot_longer(pp_ssp585_prom_mens_hist, cols = -Mes, names_to = "GCM", values_to = "Pp")

# Crea el gráfico de líneas
ggplot() +
  # Línea para cada GCM
  geom_line(data = pp_ssp585_long, aes(x = Mes, y = Pp, group = GCM), color = "blue", size = 0.5) +
  # Línea para la serie observada
  geom_line(data = pp_obs_prom_mens, aes(x = Mes, y = pp_obs_prom_mens[[2]]), color = "red", size = 1.5) +
  # Títulos y etiquetas
  labs(title = "Precipitación Mensual Promedio",
       x = "Mes del Año",
       y = "Precipitación (mm)") +
  # Ajustes de tema
  theme_minimal() +
  theme(legend.position = "none", # Elimina la leyenda automática
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  # Ajustes del eje X
  scale_x_continuous(breaks = 1:12) +
  # Añadir la leyenda manualmente con líneas representativas
  annotate("rect", xmin = 1, xmax = 4, ymin = max(pp_ssp585_long$Pp) * 1.1, 
           ymax = max(pp_ssp585_long$Pp) * 1.3, alpha = 0.5, color = "black", fill = "white") +
  annotate("segment", x = 1.2, xend = 1.4, y = max(pp_ssp585_long$Pp) * 1.25, yend = max(pp_ssp585_long$Pp) * 1.25, color = "blue", size = 0.5) +
  annotate("text", x = 1.5, y = max(pp_ssp585_long$Pp) * 1.25, 
           label = "SSP 5-8.5", color = "blue", size = 4, hjust = 0) +
  annotate("segment", x = 1.2, xend = 1.4, y = max(pp_ssp585_long$Pp) * 1.2, yend = max(pp_ssp585_long$Pp) * 1.2, color = "red", size = 1.5) +
  annotate("text", x = 1.5, y = max(pp_ssp585_long$Pp) * 1.2, 
           label = "Obs", color = "red", size = 4, hjust = 0)





