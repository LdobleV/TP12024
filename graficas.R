
library(readxl)
library(ggplot2)
library(dplyr)
###1 Grafica cercania a basurales###

# Especificar los valores de interés
valores_interes <- c("No", "Sí, a menos de 500 metros", "Sí, a más de 500 metros y menos de 2 kilómetros")

# Inicializar un vector para almacenar los conteos
conteos <- sapply(valores_interes, function(valor) sum(datos$`¿Hay basurales cerca de su vivienda?` == valor))

# Convertir el resultado a un data frame
conteo_df <- data.frame(valores = valores_interes, Frecuencia = conteos)

# Crear la gráfica de barras
ggplot(conteo_df, aes(x = valores, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Frecuencia de Apariciones por cercania a basurales",
       x = "cercania a basurales",
       y = "Frecuencia")

###2 Grafico Frecuencia de recolección de residuos###

# Especificar los valores de interés
valores_interes <- c("No hay servicio de recolección municipal", "Una vez a la semana", "Entre 2 y 4 veces a la semana", "Al menos 5 veces a la semana")

# Inicializar un vector para almacenar los conteos
conteos <- sapply(valores_interes, function(valor) sum(datos$`¿Con qué frecuencia el Municipio recolecta los residuos en sus inmediaciones?` == valor))

# Convertir el resultado a un data frame
conteo_df <- data.frame(valores = valores_interes, Frecuencia = conteos)

# Crear la gráfica de barras
ggplot(conteo_df, aes(x = valores, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Frecuencia de tiempo de recoleccion de basura", 
       x = "recolecciones por semana",
       y = "Frecuencia")

###3 Tipo de desagüe###

# Especificar los valores de interés
valores_interes <- c("Desagüe a red cloacal pública", "A pozo negro/ciego", "Desagüe a red cloacal informal/vecinal", "A cámara séptica", "No sabe")

# Inicializar un vector para almacenar los conteos
conteos <- sapply(valores_interes, function(valor) sum(datos$`¿Qué tipo de desagüe posee la vivienda?` == valor))

# Convertir el resultado a un data frame
conteo_df <- data.frame(Variable = valores_interes, Frecuencia = conteos)

# Calcular porcentajes
conteo_df$Porcentaje <- round((conteo_df$Frecuencia / sum(conteo_df$Frecuencia)) * 100, 1)

# Crear la gráfica de torta
ggplot(conteo_df, aes(x = "", y = Frecuencia, fill = Variable)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Distribución de Frecuencia por Variable") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_stack(vjust = 0.5))

###4 tipo de plagas###

#Tipo de plaga: categórica nominal: bien, faltó explicitar que es de respuesta múltiple. Gráfico de sectores: no es correcto,
#ya que la suma de las frecuencias de las categorías excede el 100% (justamente por ser de respuesta múltiple). Los gráficos de barras pueden representar las frecuencias porcentuales también.

datos$`¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`[is.na(datos$`¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`)] <- "no"        

datos$`¿Cuáles plagas?`[is.na(datos$`¿Cuáles plagas?`)] <- "no"
datos$...93[is.na(datos$...93)] <- "no"
datos$...94[is.na(datos$...94)] <- "no"

 

# Calcular el porcentaje de viviendas con plagas
porcentaje_plagas <- sum(datos$`¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?` == "Sí") / nrow(datos) * 100

# Calcular el porcentaje de viviendas con cucarachas
porcentaje_cucarachas <- sum(datos$`¿Cuáles plagas?` == "Cucarachas") / nrow(datos) * 100

# Calcular el porcentaje de viviendas con mosquitos
porcentaje_mosquitos <- sum(datos$...93 == "Mosquitos") / nrow(datos) * 100

#caluclo el porcentaje de viviendas con ratas
porcentaje_ratas <- sum(datos$...94 == "Ratas") / nrow(datos) * 100

# Crear un data frame con los resultados
resultados <- data.frame(
  Tipo = c("Plagas", "Cucarachas", "Mosquitos", "Ratas"),
  Porcentaje = c(porcentaje_plagas, porcentaje_cucarachas, porcentaje_mosquitos, porcentaje_ratas)
)

# Crear la gráfica de barras
ggplot(resultados, aes(x = Tipo, y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity",color = "black", fill = "red") +
  theme_minimal() +
  labs(title = "Porcentaje de Viviendas con Plagas",
       x = "Tipo de Plaga",
       y = "Porcentaje") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) +
  ylim(0, 100)


###5 Cantidad de personas por vivienda###
##falta mediana y dispercion


# Crear la gráfica de bastones
ggplot(datos) +
  aes(x = `¿Cuántos integrantes hay en su vivienda?`) + 
  geom_bar(width = 0.10) +
  scale_x_continuous() +
  labs(y = "Cantidad de árboles", 
       x = "Número de brotes nuevos")+
  theme_classic()



###6 Cant máxima de personas por habitación###

###7 Precio de alquiler###

# Crear el histograma
ggplot(datos, aes(x = `¿Cuál es el costo actual del mismo?`)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de Precios de Alquiler",
       x = "Precios de Alquiler",
       y = "Frecuencia")