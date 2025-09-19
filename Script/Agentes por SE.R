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

#unique(data$VSR_FINAL)
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
  filter((ANIO_FECHA_MINIMA == año_min & SEPI_FECHA_MINIMA >= SE_min) | (ANIO_FECHA_MINIMA == año_max & SEPI_FECHA_MINIMA <= SE_max)) %>% 
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología") %>%
  filter(INFLUENZA_FINAL != "Sin resultado" |  COVID_19_FINAL != "Sin resultado" | VSR_FINAL != "Sin resultado")

#tabla ESTUDIADOS Y POSITIVOS
resumen_agentes <- tabla_agentes %>%
  summarise(
    Influenza_E = sum(INFLUENZA_FINAL != "Sin resultado", na.rm = TRUE),
    Influenza_P = sum(str_detect(INFLUENZA_FINAL, regex("Influenza", ignore_case = TRUE)), na.rm = TRUE),
    SARS_E      = sum(COVID_19_FINAL != "Sin resultado", na.rm = TRUE),
    SARS_P      = sum(COVID_19_FINAL == "Positivo", na.rm = TRUE),
    VSR_E       = sum(VSR_FINAL != "Sin resultado", na.rm = TRUE),
    VSR_P       = sum(VSR_FINAL == "VSR", na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Agente", ".value"),   # separa por sufijo _E/_P
    names_pattern = "(.*)_(E|P)"
  ) %>%
  rename(Estudiados = E, Positivos = P) %>%
  mutate(
    prop = round(100 * Positivos / sum(Positivos), 1)
  ) %>%
  mutate(
    positividad = round(100 * Positivos / Estudiados, 1)
  )
#total positividad
positividad <- round(100 * sum(resumen_agentes$Positivos)/sum(resumen_agentes$Estudiados),1)

#influenza total
total_influenza <- resumen_agentes %>%
  filter(Agente == "Influenza") %>%
  pull(Positivos)
#influenza prop
prop_influenza <- resumen_agentes %>%
  filter(Agente == "Influenza") %>%
  pull(prop)
#influenza positividad
positividad_influenza <- resumen_agentes %>%
  filter(Agente == "Influenza") %>%
  pull(positividad)

#sars total
total_sars <- resumen_agentes %>%
  filter(Agente == "SARS") %>%
  pull(Positivos)
#SARS prop
prop_sars <- resumen_agentes %>%
  filter(Agente == "SARS") %>%
  pull(prop)
#sars positividad
positividad_sars <- resumen_agentes %>%
  filter(Agente == "SARS") %>%
  pull(positividad)

#vsr total
total_vsr <- resumen_agentes %>%
  filter(Agente == "VSR") %>%
  pull(Positivos)
#VSR prop
prop_vsr <- resumen_agentes %>%
  filter(Agente == "VSR") %>%
  pull(prop)
#VSR positividad
positividad_vsr <- resumen_agentes %>%
  filter(Agente == "VSR") %>%
  pull(positividad)

#TABLA LONG PARA GRAFICO
tabla_long <- tabla_agentes %>% 
  mutate(Influenza_A_H1N1 = ifelse(INFLUENZA_FINAL== "Influenza A H1N1", 1, 0),
         Influenza_A_sin_subtipificar = ifelse(INFLUENZA_FINAL== "Influenza A (sin subtipificar)", 1, 0),
         'SARS-CoV-2'     = ifelse(COVID_19_FINAL == "Positivo", 1, 0),
         VSR       = ifelse(VSR_FINAL == "VSR", 1, 0)) %>% 
  select(ANIO_FECHA_MINIMA, SEPI_FECHA_MINIMA, Influenza_A_H1N1, Influenza_A_sin_subtipificar, 'SARS-CoV-2', VSR) %>%
  pivot_longer(cols = c(Influenza_A_H1N1, Influenza_A_sin_subtipificar, 'SARS-CoV-2', VSR),
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
         Semana = factor(Semana, levels = unique(Semana[order(orden)]))) %>%
#filtro de semanas agregadas
filter((ANIO_FECHA_MINIMA == año_min & SEPI_FECHA_MINIMA >= SE_min) | (ANIO_FECHA_MINIMA == año_max & SEPI_FECHA_MINIMA <= SE_max))

# Gráfico de barras apiladas ordenado por año y semana
#colores
col_map <- c(
  "Influenza_A_H1N1"             = "#FF0000",
  "Influenza_A_sin_subtipificar" = "#FA9FB5",
  "SARS-CoV-2"                   = "#225EA8",
  "VSR"                          = "#238B45"
)
grafico_agentes_SE<- hchart(
  tabla_long,
  "column",
  hcaes(x = Semana, y = Casos, group = Agente)
) %>%
  hc_title(
    text = paste0("Agentes detectados por SE. Provincia de San Juan. Periodo: SE", SE_min,
                  "/", año_min," hasta SE", SE_max, "/", año_max)) %>%
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
  ) %>%
  hc_colors(unname(col_map))

#grafico_agentes_SE

rm(tabla_long)
rm(tabla_agentes)

