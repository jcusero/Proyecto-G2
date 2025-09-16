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
#data$EDAD_UC_IRAG <- iconv(data$EDAD_UC_IRAG, from = "latin1", to = "UTF-8")


# ----------------------------------------------------
# Exploracion de datos base de datos UCIRAG
# ----------------------------------------------------

#colnames(data) 

#data$EDAD_UC_IRAG

valores_edades <- unique(data$EDAD_UC_IRAG)
#valores_edades

# Reseteo los valores de la columna para Años




#unique(data$EDAD_UC_IRAG)



# ----------------------------------------------------
# Ordenar la variable y crear un vector para grupo de edad
# ----------------------------------------------------

orden_edades <- c(
  "0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses", "12 a 23 Meses",
  "02 a 04 Años", "05 a 09 Años", "10 a 14 Años", "15 a 19 Años",
  "20 a 24 Años", "25 a 29 Años", "30 a 34 Años", "35 a 39 Años",
  "40 a 44 Años", "45 a 49 Años", "50 a 54 Años", "55 a 59 Años",
  "60 a 64 Años", "65 a 69 Años", "70 a 74 Años", "75 y más Años"
)


data$EDAD_UC_IRAG <- factor(
  data$EDAD_UC_IRAG,
  levels = orden_edades,
  ordered = TRUE)

valores_ordenados <- levels(data$EDAD_UC_IRAG)
valores_ordenados

# ----------------------------------------------------
# Contar positivos por tipo de virus y por grupo etareo
# ----------------------------------------------------


#unique(data$INFLUENZA_FINAL)
#unique(data$VSR_FINAL)
#unique(data$COVID_19_FINAL)



# Función para clasificar los casos como "Positivo" o "Negativo"
obtener_estado_virus <- function(columna) {
  case_when(
    grepl("Influenza", columna, ignore.case = TRUE) ~ "Positivo",
    grepl("VSR", columna, ignore.case = TRUE) ~ "Positivo",
    grepl("Positivo", columna, ignore.case = TRUE) ~ "Positivo",
    TRUE ~ "Negativo"
  )
}


conteo_positivos <- data %>%
  # Agrupar el dataframe por la columna "EDAD_UC_IRAG"
  group_by(EDAD_UC_IRAG) %>%
  
  # Contar los casos positivos de cada virus en cada grupo de edad
  summarise(
    # Para Influenza, contamos los casos que contengan "Influenza"
    positivos_influenza = sum(grepl("Influenza", INFLUENZA_FINAL, ignore.case = TRUE)),
    
    # Para VSR, contamos los casos que contengan "VSR"
    positivos_vsr = sum(grepl("VSR", VSR_FINAL, ignore.case = TRUE)),
    
    # Para COVID-19, contamos los casos que sean "Positivo"
    positivos_covid_19 = sum(COVID_19_FINAL == "Positivo")
  )

# Imprimir el resultado final
#print(conteo_positivos)

# ----------------------------------------------------
# Prepara datos para graficar
# ----------------------------------------------------



# Preparar los datos para el gráfico
datos_grafico <- conteo_positivos %>%
  pivot_longer(
    cols = c(positivos_influenza, positivos_vsr, positivos_covid_19), # Columnas a pivotar
    names_to = "tipo_virus",                                          # Nueva columna para los nombres de virus
    values_to = "total_casos"                                         # Nueva columna para los valores de conteo
  ) %>%
  # Limpiar los nombres de los virus para que se vean mejor en el gráfico
  mutate(
    tipo_virus = case_when(
      tipo_virus == "positivos_influenza" ~ "Influenza",
      tipo_virus == "positivos_vsr" ~ "VSR",
      tipo_virus == "positivos_covid_19" ~ "SARS-CoV-2",
      TRUE ~ tipo_virus
    )
  )

# Ver los datos transformados
#print(datos_grafico)


# ----------------------------------------------------
# Armamos gráfico
# ----------------------------------------------------

grafico_agentes_edad <- ggplot(datos_grafico, aes(x = EDAD_UC_IRAG, y = total_casos, fill = tipo_virus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = paste0("Casos detectados por agente y grupo de edad. Provincia de San Juan.\nPeriodo: SE", SE_min,
    "/", año_min," hasta SE", SE_max, "/", año_max),
    x = "Grupo de edad",
    y = "Total de casos positivos",
    fill = "Agente"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotar las etiquetas del eje x para mejor visibilidad
  ) +
  scale_fill_manual(
    values = c(
      "Influenza"                    = rgb(255, 0, 0, maxColorValue = 255),   # rojo
      "SARS-CoV-2"                   = rgb(34,94,168, maxColorValue = 255),   # azul
      "VSR"                          = rgb(35,139,69, maxColorValue = 255)  # verde
    )
  ) 


#grafico_agentes_edad


rm(conteo_positivos)
rm(datos_grafico)
