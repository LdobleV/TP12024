library("readxl")
library("openxlsx")


rango_de_lectura <- "B3:DN1125"

# Cargo el archivo como .xlsx con el rango de lectura indicado
datos <- read_excel("Datos_LP.xlsx", range=rango_de_lectura)

#Por como esta hecha la tabla se pierden los primeros valores de las columnas 1 y 2 asi que los pongo a mano
names(datos)[1] = "PROVINCIA"
names(datos)[2] = "BARRIO"

# Ver los datos como planilla
View(datos)