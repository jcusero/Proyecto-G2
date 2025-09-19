library(dplyr)
library(leaflet)
library(geoAr)
library(readxl)

# Separar coordenadas en latitud y longitud
LISTADO_EFECTORES <- LISTADO_EFECTORES %>%
  mutate(
    lat = as.numeric(sub(",.*", "", Coordenadas)),
    lon = as.numeric(sub(".*,", "", Coordenadas))
  )

# --- Preparación de datos y mapa ---

# Obtener la geometría de la provincia de San Juan (la capa base del mapa)
san_juan_sf <- geoAr::get_geo("SAN JUAN")

# Filtrar los datos por una localidad específica
# Reemplaza "NOMBRE DE LA LOCALIDAD" con el nombre exacto de la localidad en tu Excel
LISTADO_EFECTORES_FILTRADO <- LISTADO_EFECTORES %>%
  filter(LOCALIDAD == "NOMBRE DE LA LOCALIDAD")

# Crear el mapa base y agregar el polígono de San Juan
mapa_efectores <- leaflet() %>%
  addArgTiles() %>%
  addPolygons(
    data = san_juan_sf,
    fillColor = "transparent",
    color = "#01345d",
    weight = 3,
    opacity = 1
  )

# --- Agregar solo Hospitales y Establecimientos Centinela ---

# Agregar hospitales (azules)
mapa_efectores <- mapa_efectores %>%
  addCircleMarkers(
    data = LISTADO_EFECTORES_FILTRADO %>% dplyr::filter(`tipo de establecimiento` == "HOSPITAL"),
    ~lon, ~lat,
    popup = ~paste0("<b>Nombre:</b> ", Nombre, "<br>",
                    "<b>Código SISA:</b> ", `Código SISA EFECTOR`, "<br>",
                    "<b>Nivel Complejidad:</b> ", `NIVEL COMPL`, "<br>",
                    "<b>Zona Sanitaria:</b> ", `zona sanitaria`, "<br>",
                    "<b>Localidad:</b> ", LOCALIDAD, "<br>",
                    "<b>Tipo de Establecimiento:</b> ", `tipo de establecimiento`, "<br>",
                    "<b>Estrategia:</b> ", Estrategia),
    color = "blue",
    radius = 5,
    fillOpacity = 0.7
  )

# Agregar los establecimientos centinela (cruces rojas)
mapa_efectores <- mapa_efectores %>%
  addAwesomeMarkers(
    data = LISTADO_EFECTORES_FILTRADO %>% dplyr::filter(Estrategia == "CENTINELA"),
    ~lon, ~lat,
    popup = ~paste0("<b>Nombre:</b> ", Nombre, "<br>",
                    "<b>Código SISA:</b> ", `Código SISA EFECTOR`, "<br>",
                    "<b>Nivel Complejidad:</b> ", `NIVEL COMPL`, "<br>",
                    "<b>Zona Sanitaria:</b> ", `zona sanitaria`, "<br>",
                    "<b>Localidad:</b> ", LOCALIDAD, "<br>",
                    "<b>Tipo de Establecimiento:</b> ", `tipo de establecimiento`, "<br>",
                    "<b>Estrategia:</b> ", Estrategia),
    icon = awesomeIcons(
      icon = "plus",
      library = "fa",
      markerColor = "red"
    )
  )

# Agregar leyenda personalizada
custom_legend <- htmltools::HTML('
<div style="background:white; padding: 10px; border-radius: 5px; box-shadow: 2px 2px 6px rgba(0,0,0,0.3); font-size: 14px;">
  <b>Tipos de Establecimientos</b><br>
  <i style="background:blue; width: 12px; height: 12px; display:inline-block; border-radius:50%; margin-right:6px;"></i> Hospital<br>
  <i class="fa fa-plus" style="color:red; background:white; border-radius: 50%; padding: 2px 5px; margin-right:6px;"></i> Unidad Centinela
</div>
')

mapa_efectores_sj <- mapa_efectores %>%
  addControl(custom_legend, position = "bottomright")

# Mostrar el mapa final
#mapa_efectores_sj


