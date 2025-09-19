library(readr)


# ----------------------------------------------------
# Importación de datos
# ----------------------------------------------------
# cargar base de datos
data <- read.csv("data/UC_IRAG_SAN_JUAN.csv", sep = ";")
# Cargar los datos de los efectores
LISTADO_EFECTORES <- readxl::read_excel("data/EFECTORES.xlsx")

# ----------------------------------------------------
# correccion de datos
# ----------------------------------------------------
data$CLASIFICACION_MANUAL <- iconv(data$CLASIFICACION_MANUAL, from = "latin1", to = "UTF-8")
data$EDAD_UC_IRAG <- iconv(data$EDAD_UC_IRAG, from = "latin1", to = "UTF-8")

# ----------------------------------------------------
# periodo
# ----------------------------------------------------

año_min <- 2024
año_max <- 2025
SE_min <-  23
SE_max <-  23
