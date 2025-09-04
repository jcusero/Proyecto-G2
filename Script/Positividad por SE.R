library(dplyr)
library(tidyr)
library(ggplot2)

# ----------------------------------------------------
# Importación de datos
# ----------------------------------------------------

#data <- read.csv("data/UC_IRAG_SAN_JUAN.csv", sep = ";")
#data$CLASIFICACION_MANUAL <- iconv(data$CLASIFICACION_MANUAL, from = "latin1", to = "UTF-8")



# ----------------------------------------------------
# Preparamos la base
# ----------------------------------------------------
data_filtrada <- data %>%
  filter((ANIO_FECHA_MINIMA == 2024 & SEPI_FECHA_MINIMA >= 23) | 
           (ANIO_FECHA_MINIMA == 2025 & SEPI_FECHA_MINIMA <= 23)) %>% 
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología") %>%
  select(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL)

# ----------------------------------------------------
# Pasamos a formato largo (tidy)
# ----------------------------------------------------
data_long <- data_filtrada %>%
  pivot_longer(cols = c(INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL),
               names_to = "Agente", values_to = "resultado") %>%
  filter(resultado != "Sin resultado")

# ----------------------------------------------------
# Calculamos positividad por virus y semana
# ----------------------------------------------------
positividad <- data_long %>%
  group_by(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, Agente) %>%
  summarise(
    estudiados = n(),
    positivos = sum(resultado != "Negativo"),
    .groups = "drop"
  ) %>%
  mutate(positividad = 100 * positivos / estudiados)

# ----------------------------------------------------
# Completar semanas faltantes con 0 y crear variable ordenada
# ----------------------------------------------------
positividad <- positividad %>%
  # limitar rango temporal
  filter((ANIO_FECHA_MINIMA == 2024 & SEPI_FECHA_MINIMA >= 23) | 
           (ANIO_FECHA_MINIMA == 2025 & SEPI_FECHA_MINIMA <= 23)) %>%
  # completar semanas faltantes
  complete(
    ANIO_FECHA_MINIMA,
    SEPI_FECHA_MINIMA = 1:52,
    Agente,
    fill = list(positividad = 0, estudiados = 0, positivos = 0)
  ) %>%
  # crear orden y etiqueta de semana
  mutate(
    orden = ANIO_FECHA_MINIMA * 100 + SEPI_FECHA_MINIMA,
    Semana = paste0(ANIO_FECHA_MINIMA, "-", SEPI_FECHA_MINIMA),
    Semana = factor(Semana, levels = unique(Semana[order(orden)]))
  )

# ----------------------------------------------------
# Gráfico con eje X continuo año-semana
# ----------------------------------------------------
grafico_positividad_SE <- ggplot(positividad, aes(x = Semana, y = positividad,
                        color = Agente, group = Agente)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Positividad semanal de Influenza, COVID-19 y VSR",
    x = "Semana epidemiológica (Año-Semana)",
    y = "Positividad (%)",
    color = "Agente"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)  # rotar etiquetas para que no se amontonen
  )





