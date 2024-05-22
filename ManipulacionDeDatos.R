#Este archivo debe correrse luego de la ingesta de datos, modifica la tabla a conveniencia

install.packages("dplyr")

library(dplyr)

# Tipo de desagüe

# Fijo el dataset
attach(datos)


provincias <- c('Misiones', 'Formosa', 'Jujuy', 'Tucumán', 'Santiago', 'Corrientes', 'Chaco')

#Filtro solo las provincias objetivo de estudio
datos <- datos[datos$PROVINCIA %in% provincias, ]

#Asi se ve bien en que columna esta cada plaga y no casillas vacias (Las reemplazo con un "no")

names(datos)[92] = "Cucarachas?"
names(datos)[93] = "Mosquitos?"
names(datos)[94] = "Ratas?"

datos$`¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`[is.na(datos$`¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`)] <- "no"        

datos$`Cucarachas?`[is.na(datos$`Cucarachas?`)] <- "no"
datos$`Mosquitos?`[is.na(datos$`Mosquitos?`)] <- "no"
datos$`Ratas?`[is.na(datos$`Ratas?`)] <- "no"
