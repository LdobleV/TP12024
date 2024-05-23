library(readxl)
library(ggplot2)
library(dplyr)
library(extrafont)
### Bi variado 

###Precio de alquiler y Hacinamieto###

datos <- datos %>%
  mutate(hacinamiento = `¿Cuántos integrantes hay en su vivienda?` / `¿Cuántos ambientes en su vivienda se utilizan como dormitorio?`)


#grafica
ggplot(datos, aes(x=`¿Cuál es el costo actual del mismo?`, y = hacinamiento)) +
  geom_point(color="blue", size = 3) +
  ggtitle("Diagrama de Dispersión") +
  xlab("Precio de Alquiler") +
  ylab("Hacinamiento")


###desague y plagas###


# Filtrar las viviendas con pozo negro y que presentan plagas
viviendas_pozo_negro_plagas <- datos %>%
  filter(`¿Qué tipo de desagüe posee la vivienda?` == "A pozo negro/ciego" & `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?` == "Sí") %>%
  nrow()


# Contar el número total de viviendas con pozo negro
total_viviendas_pozo_negro <- datos %>%
  filter(`¿Qué tipo de desagüe posee la vivienda?` == "A pozo negro/ciego") %>%
  nrow()

# Calcular el porcentaje
porcentaje_plagas_pozo_negro <- (viviendas_pozo_negro_plagas / total_viviendas_pozo_negro) * 100
####

# Filtrar las viviendas con pozo negro y que presentan plagas
viviendas_cloaca_plagas <- datos %>%
  filter(`¿Qué tipo de desagüe posee la vivienda?` == "Desagüe a red cloacal pública" & `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?` == "Sí") %>%
  nrow()


# Contar el número total de viviendas con pozo negro
total_viviendas_cloaca <- datos %>%
  filter(`¿Qué tipo de desagüe posee la vivienda?` == "Desagüe a red cloacal pública") %>%
  nrow()

# Calcular el porcentaje
porcentaje_plagas_cloaca <- (viviendas_cloaca_plagas / total_viviendas_cloaca) * 100

####

# Filtrar las viviendas con pozo negro y que presentan plagas
viviendas_cloaca_inf_plagas <- datos %>%
  filter(`¿Qué tipo de desagüe posee la vivienda?` == "Desagüe a red cloacal informal/vecinal" & `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?` == "Sí") %>%
  nrow()


# Contar el número total de viviendas con pozo negro
total_viviendas_cloaca_inf <- datos %>%
  filter(`¿Qué tipo de desagüe posee la vivienda?` == "Desagüe a red cloacal informal/vecinal") %>%
  nrow()

# Calcular el porcentaje
porcentaje_plagas_cloaca_inf <- (viviendas_cloaca_inf_plagas / total_viviendas_cloaca_inf) * 100

####

# Filtrar las viviendas con pozo negro y que presentan plagas
viviendas_camara_sep_plagas <- datos %>%
  filter(`¿Qué tipo de desagüe posee la vivienda?` == "A cámara séptica" & `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?` == "Sí") %>%
  nrow()


# Contar el número total de viviendas con pozo negro
total_viviendas_camara_sep <- datos %>%
  filter(`¿Qué tipo de desagüe posee la vivienda?` == "A cámara séptica") %>%
  nrow()

# Calcular el porcentaje
porcentaje_plagas_camara_sep <- (viviendas_camara_sep_plagas / total_viviendas_camara_sep) * 100

# Crear un data frame con los resultados
resultados <- data.frame(
  Tipo = c("Desagüe a red cloacal pública", "A cámara séptica", "Desagüe a red cloacal informal/vecinal", "A pozo negro/ciego"),
  Porcentaje = c(porcentaje_plagas_cloaca, porcentaje_plagas_camara_sep , porcentaje_plagas_cloaca_inf, porcentaje_plagas_pozo_negro)
)


# Crear la gráfica de barras del porcentaje de presencia de cada plaga
ggplot(resultados, aes(x = Tipo, y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity",color = "black", fill = "blue") +
  theme_minimal() +
  labs(title = "Porcentaje de plagas, segun el tipo de desague",
       x = "Tipo de desague",
       y = "Porcentaje de plagas") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) +
  ylim(0, 100)





