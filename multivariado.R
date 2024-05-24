library(readxl)
library(ggplot2)
library(dplyr)
library(extrafont)
### Bi variado 

font_import()
loadfonts(device = "win")

#Veo las fuentes que importe
fonts()

###Precio de alquiler y Hacinamieto###

datos <- datos %>%
  mutate(hacinamiento = `¿Cuántos integrantes hay en su vivienda?` / `¿Cuántos ambientes en su vivienda se utilizan como dormitorio?`)


ggplot(datos, aes(x=`¿Cuál es el costo actual del mismo?`, y = hacinamiento)) +
  theme_bw() +
  geom_point(color="blue", size = 3) +
  ggtitle("Diagrama de Dispersión") +
  xlab("Precio de Alquiler") +
  ylab("Personas durmiendo por habitación")

ggsave("Hacinamiento vs Precio del Alquiler.png", last_plot(), dpi=600)

###desague y plagas###

calcular <- function(colTotal, colCond, condColTotal, condColCond) {
  viviendas_que_cumplen_ambas <- datos %>%
    filter(colTotal == condColTotal &  colCond == condColCond) %>%
    nrow()
  
  total <- datos %>%
    filter(colTotal == condColTotal) %>%
    nrow()
  
  return((viviendas_que_cumplen_ambas / total) * 100)
}

porcentaje_plagas_pozo_negro <- calcular(`¿Qué tipo de desagüe posee la vivienda?`,
               `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
               "A pozo negro/ciego",
               "Sí")

porcentaje_plagas_cloaca <- calcular(`¿Qué tipo de desagüe posee la vivienda?`,
                                         `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                         "Desagüe a red cloacal pública",
                                         "Sí")

porcentaje_plagas_cloaca_inf <- calcular(`¿Qué tipo de desagüe posee la vivienda?`,
                                     `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                     "Desagüe a red cloacal informal/vecinal",
                                     "Sí")


porcentaje_plagas_camara_sep <- calcular(`¿Qué tipo de desagüe posee la vivienda?`,
                                         `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                         "A cámara séptica",
                                         "Sí")


# Crear un data frame con los resultados
resultados <- data.frame(
  Tipo = c("Desagüe a red cloacal pública", "A cámara séptica", "Desagüe a red cloacal informal/vecinal", "A pozo negro/ciego"),
  Porcentaje = c(porcentaje_plagas_cloaca, porcentaje_plagas_camara_sep , porcentaje_plagas_cloaca_inf, porcentaje_plagas_pozo_negro)
)


# Crear la gráfica de barras del porcentaje de presencia de cada plaga
ggplot(resultados, aes(x = Tipo, y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity",color = "black", size = 1, fill = "skyblue") +
  theme_bw() +
  labs(title = "Porcentaje de presencia plagas, segun el tipo de desague",
       x = "Tipo de desague",
       y = "Porcentaje de plagas") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) + 
  ylim(0, 100) +
  theme(text = element_text(family = "Trebuchet MS"))

ggsave("Plagas vs desague.png", last_plot(), dpi=600)

###plagas distancia a basurales###

porcentaje_plagas_sinbasurales <- calcular(`¿Hay basurales cerca de su vivienda?`,
                                         `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                         "No",
                                         "Sí")

porcentaje_plagas_500m <- calcular(`¿Hay basurales cerca de su vivienda?`,
                                   `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                    "Sí, a menos de 500 metros",
                                    "Sí")


porcentaje_plagas_2km <- calcular(`¿Hay basurales cerca de su vivienda?`,
                                   `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                   "Sí, a más de 500 metros y menos de 2 kilómetros",
                                   "Sí")

# Crear un data frame con los resultados
resultados <- data.frame(
  Tipo = c("No", "Sí, a menos de 500 metros", "Sí, a más de 500 metros y menos de 2 kilómetros"),
  Porcentaje = c(porcentaje_plagas_sinbasurales, porcentaje_plagas_500m, porcentaje_plagas_2km)
)


# Crear la gráfica de barras del porcentaje de presencia de cada plaga
ggplot(resultados, aes(x = Tipo, y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity",color = "black", size = 1, fill = "skyblue") +
  theme_bw() +
  labs(title = "Porcentaje de plagas, segun la distancia a basurales",
       x = "Distancia a basurales",
       y = "Porcentaje de plagas") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) +
  ylim(0, 100) +
  theme(text = element_text(family = "Trebuchet MS"))

ggsave("Plagas vs Basurales.png", last_plot(), dpi=600)


###plagas segun timepo de recoleccion###

porcentaje_plagas_1semana <- calcular(`¿Con qué frecuencia el Municipio recolecta los residuos en sus inmediaciones?`,
                                           `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                           "Una vez a la semana",
                                           "Sí")

porcentaje_plagas_2_4semana <- calcular(`¿Con qué frecuencia el Municipio recolecta los residuos en sus inmediaciones?`,
                                      `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                      "Entre 2 y 4 veces a la semana",
                                      "Sí")

porcentaje_plagas_5semana <- calcular(`¿Con qué frecuencia el Municipio recolecta los residuos en sus inmediaciones?`,
                                        `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                        "Al menos 5 veces a la semana",
                                        "Sí")

porcentaje_plagas_sinrecoleccion <- calcular(`¿Con qué frecuencia el Municipio recolecta los residuos en sus inmediaciones?`,
                                      `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`,
                                      "No hay servicio de recolección municipal",
                                      "Sí")

# Crear un data frame con los resultados
resultados <- data.frame(
  Tipo = c("No hay servicio de recolección municipal", "Una vez a la semana", "Entre 2 y 4 veces a la semana", "Al menos 5 veces a la semana"),
  Porcentaje = c(porcentaje_plagas_sinrecoleccion, porcentaje_plagas_1semana, porcentaje_plagas_5semana, porcentaje_plagas_sinrecoleccion)
)


# Crear la gráfica de barras del porcentaje de presencia de cada plaga
ggplot(resultados, aes(x = Tipo, y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity",color = "black", size = 1, fill = "skyblue") +
  theme_bw() +
  labs(title = "Porcentaje de plagas, segun frecuencia de recoleccion de basura",
       x = "Frecuencia de recoleccion",
       y = "Porcentaje de presencia de plagas") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) + 
  ylim(0, 100) +
  theme(text = element_text(family = "Trebuchet MS"))

ggsave("Plagas vs Frecuencia de Recoleccion.png", last_plot(), dpi=600)

### Grafico de presencia de plagas segun precio del alquiler

# Filtrar los datos para eliminar las filas con NA en precio_de_alquiler
datos1 <- datos %>%
  filter(!is.na(`¿Cuál es el costo actual del mismo?`))

# Crear los boxplots usando ggplot2
ggplot(datos1, aes(x = `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`, y = `¿Cuál es el costo actual del mismo?`, fill = `¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?`)) +
  geom_boxplot(width = 0.30, fill = "lightgray", outlier.size = 1) +
  ggtitle("Precio de Alquiler vs Presencia de Plagas") +
  xlab("Presencia de Plagas") +
  ylab("Precio de Alquiler") +
  theme_bw() +
  theme(text = element_text(family = "Trebuchet MS"))

ggsave("Plagas vs Precio del Alquiler.png", last_plot(), dpi=600)

