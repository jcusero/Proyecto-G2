#PROYECTO-G2

Este proyecto sirve para generar un reporte epidemiologico descriptivo de la UCIRAG.

# OBJETIVOS
-Almacenar la informacion requerida para ejecutar en RSTUDIO los procedimientos para hacer el informe de manera automatizada.
-Trabajar en grupos de manera colaborativa para agregar nuevas funcionalidades al programa.

# DESCRIPCION
El analisis de la informacion generada por la unidad centinela UCIRAG tiene como objetivo principal obtener informacion para la toma de desiciones.

# ESTRUCTURA DEL REPOSITORIO

.gitignore  - archivo que contiene las especificaciones de los documentos que no deben subirse al repositorio

.Rhistory - archivo del historial de Rstudio para este proyecto

.RData   - archivo del proyecto

Informe UCIRAG SAN JUAN - LA PAMPA - CHACO.qmd - archivo quarto que contiene el texto y los graficos del informe, asi como el formato y estilo.

Proyecto-G2 - Archivo del proyecto de Rstudio

README - Paso a paso del uso del repositorio github

styles - archivo de estilo del informe

Archivos/ - carpeta que contiene documentos que aportan informacion adicional sobre la UCIRAG

Output/ - carpeta de archivos de salida como html, imagenes, etc

Script/ - carpeta que contiene los procedimientos en lenguaje R que utiliza el programa para el analisis y la generacion de datos, tablas, graficos, etc.

# REQUISITOS

-Hardware: Procesador de 2 nucleos, I4 o supeprior. Sistema de 64-bits preferiblemente, con un minimo 4-8 GB de RAM para datasets pequeños.
-Software: 
-SO: Windows, MacOS o Linux
-GIT
-RSTUDIO
-Explorador web

# PASO A PASO

Clonar el repositorio (primera vez)

1-clonar este repositorio en tu disco local (ver archivos/instructivo de GIT HUB)

2-generar una carpeta adicional con el nombre "data" en la carpeta raiz del repositorio clonado en el disco local.

Generar un informe

1-Colocar una base de datos en formato csv con la informacion sobre UCIRAG como insumo del programa.

2-Controlar los campos de la base de datos segun las variables analizadas, como indice el plan de analisis (ver archivos/plan de analisis).

3-Verificar nombre de la base de datos, asi como los limites temporales en el archivo Script/Carga de datos.R y actualizar si es necesario

4-Abrir el arhivo qmd

5-Correr el codigo del qmd

6-Analizar los datos y modificar el texto descriptivo

7-Renderizar el documento

8-El informe se genera en formato html dentro de la carpeta raiz del proyecto.


Modificar un procedimiento

Seguir los pasos indicados por el instructivo para subir un cambio al repositorio (ver archivos/instructivo de GIT HUB)
Abrir Pull Request en GitHub

