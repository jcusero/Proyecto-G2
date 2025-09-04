library(readr)


# ----------------------------------------------------
# Importación de datos
# ----------------------------------------------------

data <- read.csv("data/UC_IRAG_SAN_JUAN.csv", sep = ";")


# ----------------------------------------------------
# correccion de datos
# ----------------------------------------------------

data$CLASIFICACION_MANUAL <- iconv(data$CLASIFICACION_MANUAL, from = "latin1", to = "UTF-8")
data$EDAD_UC_IRAG <- iconv(data$EDAD_UC_IRAG, from = "latin1", to = "UTF-8")
