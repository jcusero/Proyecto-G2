library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(writexl)
library(readr)
library(dplyr)
library(leaflet)
library(geoAr)
library(tidyr)



# ----------------------------------------------------
# Importación de datos
# ----------------------------------------------------

#data <- read.csv("data/UC_IRAG_SAN_JUAN.csv", sep = ";")
#data$CLASIFICACION_MANUAL <- iconv(data$CLASIFICACION_MANUAL, from = "latin1", to = "UTF-8")

# ----------------------------------------------------
#eploracion de datos
# ----------------------------------------------------

#data$CLASIFICACION_MANUAL
#cuenta_variable <- data %>% 
 # group_by(CLASIFICACION_MANUAL) %>% 
#  summarise(CASOS = n())
#cuenta_variable

# ----------------------------------------------------
#seleccion de datos y filtros
# ----------------------------------------------------

# Select: seleccionar columnas
tabla_agentes <- data %>%
  select(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, CLASIFICACION_MANUAL,INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL) %>% 
  filter((ANIO_FECHA_MINIMA == 2024 & SEPI_FECHA_MINIMA >= 23) | (ANIO_FECHA_MINIMA == 2025 & SEPI_FECHA_MINIMA <= 23)) %>% 
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología") %>%
  filter(str_starts(INFLUENZA_FINAL, "Influenza") | COVID_19_FINAL == "Positivo" | VSR_FINAL == "VSR")

# Supongamos que ya tenés tabla_agentes filtrada como antes
# Primero conviene llevar los agentes a formato "largo"
tabla_long <- tabla_agentes %>% 
  mutate(Influenza = ifelse(str_starts(INFLUENZA_FINAL, "Influenza"), 1, 0),
         Covid     = ifelse(COVID_19_FINAL == "Positivo", 1, 0),
         VSR       = ifelse(VSR_FINAL == "VSR", 1, 0)) %>% 
  select(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, Influenza, Covid, VSR) %>%
  pivot_longer(cols = c(Influenza, Covid, VSR),
               names_to = "Agente",
               values_to = "Casos") %>%
  group_by(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, Agente) %>%
  summarise(Casos = sum(Casos), .groups = "drop") %>%
  # Completar semanas faltantes con 0
  complete(ANIO_FECHA_MINIMA,SEPI_FECHA_MINIMA = 1:52,Agente,
    fill = list(Casos = 0)
  ) %>%
  # Crear variable de orden y factor para el eje X
  mutate(orden = ANIO_FECHA_MINIMA * 100 + SEPI_FECHA_MINIMA,
         Semana = paste0(ANIO_FECHA_MINIMA, "-", SEPI_FECHA_MINIMA),
         Semana = factor(Semana, levels = unique(Semana[order(orden)])))

tabla_long <- tabla_long %>%
  filter((ANIO_FECHA_MINIMA == 2024 & SEPI_FECHA_MINIMA >= 23) | (ANIO_FECHA_MINIMA == 2025 & SEPI_FECHA_MINIMA <= 23))


# Gráfico de barras apiladas ordenado por año y semana
grafico_agentes_SE <- ggplot(tabla_long, aes(x = Semana, y = Casos, fill = Agente)) +
  geom_col() +
  labs(title = paste0("Agentes detectados por SE. Provincia de San Juan.","\n","Periodo: SE23/",
                      min(tabla_long$ANIO_FECHA_MINIMA)," hasta SE23/",
       max(tabla_long$ANIO_FECHA_MINIMA),"."),
       x = "Semana epidemiológica (Año-Semana)",
       y = "Cantidad de casos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(tabla_long)
rm(tabla_agentes)

