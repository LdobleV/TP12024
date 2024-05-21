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

datos$`¿Cuáles plagas?`[is.na(datos$`¿Cuáles plagas?`)] <- "no"
datos$...93[is.na(datos$...93)] <- "no"
datos$...94[is.na(datos$...94)] <- "no"
