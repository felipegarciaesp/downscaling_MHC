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
    group_by(Mes = month(df[[column]])) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE))
}

# 5) Funcion para graficar las series mensuales:
# Esta funcion la utilizo para graficar las series mensuales de los datos
# observados junto con la de las proyecciones.
# Para que esta funcion sirva es necesario agregar dos dataframes: uno con la data
# observada y otro con la data de las proyecciones. Ambos deben tener como primera 
# columna los numeros de los meses, del 1 al 12.
graphic_month <- function(df_obs, df_gcm, meses, title) {
  # Convierte el dataframe de los GCM a formato largo
  df_gcm_long <- pivot_longer(df_gcm, cols = -all_of(meses), names_to = "GCM", values_to = "Pp")
  
  # Crea el gráfico de líneas
  ggplot() +
    # Línea para cada GCM
    geom_line(data = df_gcm_long, aes_string(x = meses, y = "Pp", group = "GCM"), color = "blue", size = 0.5) +
    # Línea para la serie observada
    geom_line(data = df_obs, aes_string(x = meses, y = colnames(df_obs)[2]), color = "red", size = 1.5) +
    # Títulos y etiquetas
    labs(title = title,
         x = "Mes del Año",
         y = "Precipitación (mm)") +
    # Ajustes de tema
    theme_minimal() +
    theme(legend.position = "none", # Elimina la leyenda automática
          axis.text.x = element_text(angle = 0, hjust = 1),
          plot.title = element_text(hjust = 0.5)) + # Centra el título
    # Ajustes del eje X
    scale_x_continuous(breaks = 1:12) +
    # Añadir la leyenda manualmente con líneas representativas
    annotate("rect", xmin = 1, xmax = 4, ymin = max(df_gcm_long$Pp) * 1.1, 
             ymax = max(df_gcm_long$Pp) * 1.3, alpha = 0.5, color = "black", fill = "white") +
    annotate("segment", x = 1.2, xend = 1.4, y = max(df_gcm_long$Pp) * 1.25, yend = max(df_gcm_long$Pp) * 1.25, color = "blue", size = 0.5) +
    annotate("text", x = 1.5, y = max(df_gcm_long$Pp) * 1.25, 
             label = "SSP 5-8.5", color = "blue", size = 4, hjust = 0) +
    annotate("segment", x = 1.2, xend = 1.4, y = max(df_gcm_long$Pp) * 1.2, yend = max(df_gcm_long$Pp) * 1.2, color = "red", size = 1.5) +
    annotate("text", x = 1.5, y = max(df_gcm_long$Pp) * 1.2, 
             label = "Obs", color = "red", size = 4, hjust = 0)
}

# 6) Funcion para obtener las desviaciones estándar de un dataframe:
std_mon <- function(df,column) {
  df_std_mon <- df %>%
    group_by(Mes = month(df[[column]])) %>%
    summarize(across(where(is.numeric), sd, na.rm = TRUE))
}

# 7) Funcion para cambiar a numerical una columna especifica:
convert_numeric <- function(df) {
  df$pr_Amon_NorESM2_LM_r1i1p1f1_gn <- as.numeric(as.character(df$pr_Amon_NorESM2_LM_r1i1p1f1_gn))
  return(df)
}

# 8) Funcion para caclular Delta Change:
# ID igual a 'pr' o 'tas':
delta_change <- function(gcm_hist, gcm_fut, ID) {
  if (ID=='pr') {
    df <- (gcm_fut-gcm_hist)/gcm_hist
  } else {
    df <- gcm_fut - gcm_hist
  }
  return(df)
  
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

# IDEAS PARA MEJORAR EL CODIGO:
# QUE LOS DATAFRAMES QUE SE OBTENGAN TENGAN LAS FECHAS O LOS MESES EN EL INDICE
# Y QUE NO SEA UNA COLUMNA COMO ESTÁ ACTUALMENTE. LUEGO, EL CODIGO QUE VIENE
# DEBE ESTAR EN FUNCION DE ESTA MODIFICACION.
# SERIA BUENA IDEA SER MAS ORDENADO Y HACER UN DATAFRAME QUE TENGA LOS VALORES
# DE LAS PROYECCIONES SSP245, PERO QUE LA PRIMERA COLUMNA TENGA LOS DATOS OBSERVADOS
# HACER LO MISMO CON EL SSP585.

# Ocupamos la funcion filtrado para filtrar para las fechas de interes:
pp_obs <- filtrado(pp_obs, 'Fechas', start_hist, end_hist)
pp_ssp245_hist <- filtrado(pp_ssp245, 'V1', start_hist, end_hist)
pp_ssp585_hist <- filtrado(pp_ssp585, 'V1', start_hist, end_hist)
pp_ssp245_fut <- filtrado(pp_ssp245, 'V1', start_fut, end_fut)
pp_ssp585_fut <- filtrado(pp_ssp585, 'V1', start_fut, end_fut)

# PARA ESTE CASO PARTICULAR SE SABE QUE LOS DATOS DEL MODELO 'pr_Amon_NorESM2_LM_r1i1p1f1_gn'
# SE ESTÁN TOMANDO COMO 'chr' EN VEZ DE 'num' DEBIDO A QUE LOS PRIMEROS DATOS SON NA
# EN EL EXCEL. POR SIMPLICIDAD VOY A CONVERTIR LA DATA DE ESTE COLUMNA A NUMERIC, PERO DEBO
# TRABAJAR EN DOS COSAS: 
# 1. VERIFICAR SI CUANDO TRABJE CON ARCHIVO NETCDF ESTE PROBLEMA SE VA A PRESENTAR IGUAL.
# 2. HALLAR UNA FORMA DE QUE SE CONVIERTAN TODAS LAS COLUMNAS DEL DATAFRAME A NUMERICAL, SIN
# CAMBIAR A NUMERICAL LAS FECHAS. UNA OPCION PODRIA SER DEJAR LAS FECHAS EN EL INDICE Y NO
# COMO UNA COLUMNA MAS.
# PARA ESTE CASO PARTICULAR, CREARE UNA FUNCION PARA HACER ESTO.
pp_ssp245_hist<- convert_numeric(pp_ssp245_hist)
pp_ssp585_hist<- convert_numeric(pp_ssp585_hist)
pp_ssp245_fut<- convert_numeric(pp_ssp245_fut)
pp_ssp585_fut<- convert_numeric(pp_ssp585_fut)

# Obtenemos las precipitaciones medias mensuales de las serie observada y para
# la serie historica de ambas proyecciones ssp:
pp_obs_prom_mens <- mean_mon(pp_obs, 'Fechas')
pp_ssp245_prom_mens_hist <- mean_mon(pp_ssp245_hist, 'V1')
pp_ssp585_prom_mens_hist <- mean_mon(pp_ssp585_hist, 'V1')

# Obtenemos las precipitaciones medias mensuals de las proyecciones para el periodo
# futuro:
pp_ssp245_prom_mens_fut <- mean_mon(pp_ssp245_fut, 'V1')
pp_ssp585_prom_mens_fut <- mean_mon(pp_ssp585_fut, 'V1')

# Obtenemos la desviacion estándar mensual de la serie observada y para la serie
# historica de ambas proyecciones ssp:
pp_obs_std_mens <- std_mon(pp_obs, 'Fechas')
pp_ssp245_std_mens_hist <- std_mon(pp_ssp245_hist, 'V1')
pp_ssp585_std_mens_hist <- std_mon(pp_ssp585_hist, 'V1')

# OBTENCION DE DELTA CHANGE:
# 1. Se obtienen los promedios mensuales de los GCM para los periodos futuro e historico:
pp_gcm_fut <- pp_ssp585_prom_mens_fut[,-1]
pp_gcm_hist <- pp_ssp585_prom_mens_hist[,-1]

# 2. Se calcula el delta change de precipitaciones con la funcion delta_change:
dlt_chng_ssp585 <- delta_change(pp_gcm_hist,pp_gcm_fut,'pr')

# 3. Se confecciona la version larga del dataframe para poder graficarla
dlt_chng_ssp585 <- dlt_chng_ssp585 %>%
  mutate(Index = 1:n()) %>%
  pivot_longer(cols=-Index,names_to='Variable',values_to='Value')
  




# Graficos:

# Se muestra grafico de precipitaciones medias mensuales observadas y de ssp585:
title = 'Ejercicio 1: Promedios Mensuales SSP5-8.5'
graphic_month(pp_obs_prom_mens, pp_ssp585_prom_mens_hist, 'Mes', title)

# Se muestra grafico de desviaciones estandar mensuales observadas y de ssp585:
title = 'Ejercicio 2: Desv. Est. Mnesuales SSP5-8.5'
graphic_month(pp_obs_std_mens, pp_ssp585_std_mens_hist, 'Mes', title)

# Se crea grafico para el delta change:
ggplot() +
  geom_line(data = dlt_chng_ssp585, aes_string(x = 'Index', y = 'Value', group = 'Variable'), color = 'blue', size = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  labs(title = "Ejercicio 3: Delta Change SSP5-8.5",
       x = "Índice",
       y = "Valor") +
  theme_minimal() +
  theme(legend.position = 'none') + # Se elimina la leyenda.
  ylim(-1.5,1.5) + #Se ajusta rango del eje Y.
  scale_x_continuous(breaks = 1:12)

