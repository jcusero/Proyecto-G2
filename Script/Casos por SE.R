library(dplyr)
library(stringr)
library(readr)
library(leaflet)
library(tidyr)
library(highcharter)



# ----------------------------------------------------
# Importación de datos
# ----------------------------------------------------

#data <- read.csv("data/UC_IRAG_SAN_JUAN.csv", sep = ";")
#data$CLASIFICACION_MANUAL <- iconv(data$CLASIFICACION_MANUAL, from = "latin1", to = "UTF-8")

# ----------------------------------------------------
#eploracion de datos
# ----------------------------------------------------

#unique(data$CLASIFICACION_MANUAL)
# cuenta_variable <- data %>% 
# group_by(CLASIFICACION_MANUAL) %>% 
# summarise(CASOS = n())
#cuenta_variable

# ----------------------------------------------------
#seleccion de datos y filtros
# ----------------------------------------------------

# Select: seleccionar columnas
tabla_clasificaciones <- data %>%
  select(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, CLASIFICACION_MANUAL) %>% 
  filter((ANIO_FECHA_MINIMA == año_min & SEPI_FECHA_MINIMA >= SE_min) | (ANIO_FECHA_MINIMA == año_max & SEPI_FECHA_MINIMA <= SE_max)) %>% 
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología") 

# contar totales
#frecuencias absolutas y relativas 
frecuencia_clasificaciones <- tabla_clasificaciones %>% count(CLASIFICACION_MANUAL) %>% 
  mutate(prop = round(100 * n / sum(n), 1)) # porcentaje con 1 decimal

# Supongamos que ya tenés tabla_agentes filtrada como antes
# Primero conviene llevar los agentes a formato "largo"
tabla_long <- tabla_clasificaciones %>% 
  mutate(IRAG = ifelse(CLASIFICACION_MANUAL== "Infección respiratoria aguda grave (IRAG)", 1, 0),
         IRAG_extendida = ifelse(CLASIFICACION_MANUAL== "IRAG extendida", 1, 0)) %>% 
  select(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, IRAG, IRAG_extendida) %>%
  pivot_longer(cols = c(IRAG, IRAG_extendida),
               names_to = "Clasificacion",
               values_to = "Casos")  %>% 
  group_by(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, Clasificacion) %>%
  summarise(Casos = sum(Casos), .groups = "drop") %>%
  
  # Completar semanas faltantes con 0
  complete(ANIO_FECHA_MINIMA,SEPI_FECHA_MINIMA = 1:52,Clasificacion,
           fill = list(Casos = 0)
  ) %>%
  # Crear variable de orden y factor para el eje X
  mutate(orden = ANIO_FECHA_MINIMA * 100 + SEPI_FECHA_MINIMA,
         Semana = paste0(ANIO_FECHA_MINIMA, "-", SEPI_FECHA_MINIMA),
         Semana = factor(Semana, levels = unique(Semana[order(orden)]))) %>%
  #filtro de semanas agregadas
  filter((ANIO_FECHA_MINIMA == año_min & SEPI_FECHA_MINIMA >= SE_min) | (ANIO_FECHA_MINIMA == año_max & SEPI_FECHA_MINIMA <= SE_max))

# Gráfico de barras apiladas ordenado por año y semana
grafico_casos_SE <- hchart(
  tabla_long,
  "column",
  hcaes(x = Semana, y = Casos, group = Clasificacion)
) %>%
  hc_title(
    text = paste0("Casos de IRAG e IRAG extendida por SE. Provincia de San Juan. Periodo: SE", SE_min,
                  "/", año_min," hasta SE", SE_max, "/", año_max)
  ) %>%
  hc_xAxis(
    title = list(text = "Semana epidemiológica (Año-Semana)"),
    labels = list(rotation = -90)
  ) %>%
  hc_yAxis(
    title = list(text = "Frecuencia de casos")
  ) %>%
  hc_plotOptions(
    column = list(stacking = "normal")   # apilado
  ) %>%
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE
  )

grafico_casos_SE

rm(tabla_long)
rm(tabla_agentes)
