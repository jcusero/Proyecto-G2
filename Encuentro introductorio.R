#=====================================================
# Preparación de la sesión
#=====================================================

# ----------------------------------------------------
# Instalar paquetes (solo una vez)
# ----------------------------------------------------

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("readxl")
# install.packages("writexl")
# install.packages("readr")

# ----------------------------------------------------
# Cargar librerías
# ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(writexl)
library(readr)


# ================================================
# Manejo de objetos
# ================================================

# En R, toda la información se almacena en objetos. Los objetos pueden ser de
# distintos tipos: variables simples, vectores, data frames, listas, factores,
# etc. A continuación, ejemplos básicos de cada uno:

# Variables
edad <- 25
print(edad)

edad

# Puedo reescribir esa variable o crear una nueva

edad <- edad + 2
edad


edad3 <- edad + 3
edad3



a <- 10               # numérico
A <- "meses"          # carácter
b <- TRUE             # lógico
c <- Sys.Date()       # fecha


# Inspeccionar objeto
# str(A)    # Muestra que es un carácter y cuál es su contenido
#class(A)  # Solo dice que es de clase "character"


str(a); class(a)
str(A); class(A)
str(b); class(b)
str(c); class(c)

# Vectores
v1 <- c(1,2,3,4)
v2 <- c(v1, 7,8,9)
nombres <- c("Carlos", "Luisa", "Bruno", "Ana")

# Data frame
# se usa para almacenar y trabajar con datos en forma de tabla.

df_ejemplo <- data.frame(
  nombre = c("Ana", "Bruno", "Carlos"),
  edad = c(25, 30, 28),
  vacunado = c(TRUE, FALSE, TRUE)
)
print(df_ejemplo)
str(df_ejemplo)

# Lista: estructura que puede contener diferentes tipos de objetos
# Es útil para agrupar resultados o elementos relacionados entre sí
lista_ejemplo <- list(nombre = "Ana", edad = 25, vacunado = TRUE)
print(lista_ejemplo)
str(lista_ejemplo)



# Lista: estructura que puede contener diferentes tipos de objetos
# Es útil para agrupar resultados o elementos relacionados entre sí

# Las listas pueden contener vectores, data frames, funciones u otras listas
# Además, cada elemento puede tener diferente longitud o estructura


lista_ejemplo_2 <- list(
  nombre = c("Ana", "Bruno"),
  edad = c(25, 30),
  vacunado = c(TRUE, FALSE),
  observaciones = list("sin síntomas", "con fiebre"),
  resumen = data.frame(fecha = Sys.Date(), evento = "Internado")
)


print(lista_ejemplo_2)
str(lista_ejemplo_2)


# Factor: se usa para representar variables categóricas con niveles definidos R
# lo usa como una variable categórica. Tiene niveles explícitos (categorías
# únicas). Es más eficiente para modelos y resúmenes. Es muy útil cuando
# queremos trabajar con categorías como grupos de edad

grupo_edad <- factor(c("0-5", "6-11", "12-24", "0-5", "12-24"))
print(grupo_edad)
str(grupo_edad)




# ----------------------------------------------------
# Importación de datos
# ----------------------------------------------------


# data <- read_csv("data/VR_NOMINAL_EVENTOCASO_anonimizado.csv")
data <- read_excel("data/VR_NOMINAL_EVENTOCASO_anonimizado.xlsx")

# Observo el directorio de trabajo
getwd()
#problems(data)


# ================================================
# Exploración de datos
# ================================================



spec(data)           # Estructura del csv
#str(data)                # Estructura del excel
dim(data)                # Dimensión del dataset
names(data)              # Nombres de columnas
colnames(data)           # Otra forma de ver los nombres
head(data, 10)           # Primeras 10 filas
tail(data, 10)           # Últimas 10 filas

# Resumen de una variable
unique(data$EVENTO)      # Valores únicos
table(data$EVENTO)       # Frecuencia por categoría



# Verificación de valores faltantes
any(is.na(data$FECHA_CONSULTA))
sum(is.na(data$FECHA_CONSULTA))

# ================================================
# Tipos de datos y transformaciones
# ================================================

class(data$DEPARTAMENTO_RESIDENCIA)
class(data$FECHA_APERTURA)
class(data$`SINTOMA_Insuficiencia respiratoria`)

# Transformar FECHA_NACIMIENTO en formato Date
data$FECHA_NACIMIENTO <- as.Date(data$FECHA_NACIMIENTO)
class(data$FECHA_NACIMIENTO)

# Verificar NA
is.na(data)
sum(is.na(data))


# ================================================
# Operaciones básicas con dplyr

# En dplyr, muchas operaciones no modifican el objeto original, 
# sino que crean uno nuevo.

# Podemos:
# - Crear un nuevo objeto usando `<-`, sin afectar el original.
# - Reemplazar un objeto existente asignándole un nuevo valor (sobrescribir).
#
# Ejemplo:

# nuevo_df <- data %>% filter(...)   # Crea uno nuevo
# data <- data %>% filter(...)       # Sobrescribe el original


# ------------------------------------------------
# Uso del pipe (%>%)
# ------------------------------------------------

# El operador pipe (%>%) permite encadenar funciones de manera legible.
# En lugar de escribir funciones anidadas como:
#   filter(select(data, EVENTO), EVENTO == "...")
# Podemos usar el pipe para que el flujo de datos sea más claro:
#   data %>% select(EVENTO) %>% filter(EVENTO == "...")
# La estructura es: objeto %>% función1 %>% función2 %>% ...
# Se lee como: "A esto le hago esto, y después le hago esto otro"

# ================================================

# Select: seleccionar columnas
select_data <- data %>% 
  select(DEPARTAMENTO_RESIDENCIA, EVENTO)


# ---------------------------------------------------

# Filter: filtrar según condición

# Podemos usar diferentes operadores lógicos (booleanos) para filtrar:
# - ==  igual a
# - !=  distinto de
# - >   mayor que
# - <   menor que
# - >=  mayor o igual que
# - <=  menor o igual que
# - %in% pertenece a
# - !is.na() no es NA

# Ejemplos:

# 1. Filtrar solo los casos exactamente iguales a una categoría específica
filtro_igual <- data %>% filter(EVENTO == "Internado y/o fallecido por COVID o IRA")


# 2. Filtrar todos los casos distintos a esa categoría específica
filtro_distinto <- data %>% filter(EVENTO != "Internado y/o fallecido por COVID o IRA")


# 3. Filtrar por pertenencia a un conjunto de valores (más de uno)
filtro_contenido <- data %>% filter(
  EVENTO %in% c(
    "Internado y/o fallecido por COVID o IRA",
    "COVID-19, Influenza y OVR en ambulatorios (No UMAs)"
  )
)

# Números
filtro_mayor <- data %>% filter(EDAD_ACTUAL > 60)

filtro_menor_igual <- data %>% filter(EDAD_ACTUAL <= 5)


# ---------------------------------------------------

# Valores faltantes filtro_completos será un nuevo data frame con solo las filas
# donde FECHA_CONSULTA tiene un valor válido (no faltante).



filtro_completos <- data %>% filter(!is.na(FECHA_CONSULTA))

#is.na(FECHA_CONSULTA) devuelve TRUE si la celda está vacía.
#!is.na(FECHA_CONSULTA) invierte eso, y devuelve TRUE si sí hay un dato.
#filter(...) mantiene solo las filas donde la condición es verdadera.


# ---------------------------------------------------

# Group_by + summarise: agrupar y contar

# - `group_by(DEPARTAMENTO_RESIDENCIA)`: agrupa las filas por cada valor
# distinto en la columna DEPARTAMENTO_RESIDENCIA.

# - `summarise(CASOS = n())`: cuenta cuántas filas (casos) 
# hay en cada grupo y guarda el resultado en una nueva columna llamada CASOS.

# El resultado es un nuevo data frame con una fila por 
# grupo (departamento) y la cantidad de casos en cada uno.

resumen_eventos <- filtro_igual %>% 
  group_by(DEPARTAMENTO_RESIDENCIA) %>% 
  summarise(CASOS = n())


# IMPORTANTE: luego de summarise(), el objeto resultante puede seguir 
# "agrupado" internamente. Si no querés que mantenga esa agrupación 
# (porque puede afectar operaciones posteriores), podés usar:
#   summarise(CASOS = n(), .groups = "drop")



# ---------------------------------------------------

# Arrange: ordenar de mayor a menor según cantidad de casos
resumen_eventos_ordenado <- resumen_eventos %>% 
  arrange(desc(CASOS))





# ================================================
# Exportación de datos
# ================================================

write_xlsx(resumen_eventos_ordenado, "salidas/resumen_eventos_ordenado.xlsx")
write.table(resumen_eventos_ordenado, "salidas/resumen_eventos_ordenado.csv", sep = ";", quote = FALSE, row.names = FALSE)





# ================================================
# Visualización
# ================================================

# Gráfico por departamento
conteo_deptos <- data %>% 
  filter(EVENTO == "Internado y/o fallecido por COVID o IRA") %>% 
  count(DEPARTAMENTO_RESIDENCIA)


# Crear gráfico de barras

ggplot(conteo_deptos, aes(x = DEPARTAMENTO_RESIDENCIA, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
#  coord_flip() +
  labs(title = "Eventos por Departamento", x = "Departamento", y = "Cantidad") +
  theme_minimal()


# Para reordenar

ggplot(conteo_deptos, aes(x = reorder(DEPARTAMENTO_RESIDENCIA, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Eventos por Departamento", x = "Departamento", y = "Cantidad") +
  theme_minimal()


