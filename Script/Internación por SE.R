#SCRIPT PARA GRAFICAR INTERNACIÓN POR SE
# ----------------------------------------------------
# Librerías
# ----------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# ----------------------------------------------------
# Importación de datos base de datos UCIRAG
# ----------------------------------------------------

#data <- read.csv("data/UC_IRAG_SAN_JUAN.csv", sep = ";")

# ----------------------------------------------------
# Exploracion de datos base de datos UCIRAG
# ----------------------------------------------------

#Ver nombres de las columnas
colnames(data) 

#Flitrar las SE desde SE23 2024 hasta SE23 2025

datos.filtrados <- data %>% filter(ANIO_MIN_INTERNACION %in% c(2024) & SEPI_FECHA_MINIMA >= 23 |
                                  ANIO_MIN_INTERNACION %in% c(2025) & SEPI_FECHA_MINIMA <= 23)

#Ver valores filtrados de ambas variables
datos.filtrados$ANIO_MIN_INTERNACION
datos.filtrados$SEPI_FECHA_MINIMA

#Creo otro data frame solo con las dos columnas que me interesan graficar
INTER_SE <- datos.filtrados %>% select(SEPI_FECHA_MINIMA, ANIO_MIN_INTERNACION)

# ----------------------------------------------------
# Ordenar los valores de semanas epidemiológicas
# ----------------------------------------------------

INTER_SE_ordenado <- INTER_SE %>% arrange(ANIO_MIN_INTERNACION, SEPI_FECHA_MINIMA)

# Ver resultado
#print(INTER_SE_ordenado)

# Contar número de internados por semana y año
frecuencia <- INTER_SE_ordenado %>%
  group_by(ANIO_MIN_INTERNACION, SEPI_FECHA_MINIMA) %>%
  summarise(internados = n()) %>%
  ungroup()

# Imprimir el resultado final
    #print(frecuencia)
    
# Crear la etiqueta tipo "2024-W23"
frecuencia <- frecuencia %>%
      mutate(semana_epi = paste0(ANIO_MIN_INTERNACION, "-SE", sprintf("%02d", SEPI_FECHA_MINIMA)))

# Ordenar factor para que ggplot respete el orden temporal
frecuencia$semana_epi <- factor(frecuencia$semana_epi, levels = unique(frecuencia$semana_epi))

#GRÁFICO
grafico_internacion_SE <- ggplot(frecuencia, aes(x = semana_epi, y = internados)) +
  geom_col(fill = "orange") +  # Un solo color para todas las semanas
  labs(
    title = "Total de Internados por Semana Epidemiológica",
    x = "Semana Epidemiológica",
    y = "Frecuencia de Internados"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

rm(frecuencia)
rm(INTER_SE)
rm(INTER_SE_ordenado)
rm(datos.filtrados)

######################################################################


