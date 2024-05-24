library(readxl)
library(ggplot2)
library(dplyr)
library(extrafont)

#Cuidado con esto Eliseo capaz tu compu explota
font_import()
loadfonts(device = "win")

#Veo las fuentes que importe
fonts()


###1 Grafica cercania a basurales###

# Especificar los valores de interés
valores_interes <- c("No", "Sí, a menos de 500 metros", "Sí, a más de 500 metros y menos de 2 kilómetros")
valores_interes <- factor(valores_interes, levels = valores_interes)

# Inicializar un vector para almacenar los conteos
conteos <- sapply(valores_interes, function(valor) sum(datos$`¿Hay basurales cerca de su vivienda?` == valor))

# Convertir el resultado a un data frame
conteo_df <- data.frame(valores = valores_interes, Frecuencia = conteos)


# Crear la gráfica de barras
ggplot(conteo_df, aes(x = valores, y = Frecuencia, order = valores)) +
  theme_bw() +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", size = 0.5) +
  ggtitle("Frecuencia de Apariciones por cercania a basurales") +
  xlab("Cercanía a basurales") + ylab("Frecuencia en viviendas") +
  theme(text = element_text(size = 14, family = "Trebuchet MS"))

ggsave("Cercania a basurales.png", last_plot(), dpi=600)


###2 Grafico Frecuencia de recolección de residuos###

# Especificar los valores de interés
valores_interes <- c("No hay servicio de recolección municipal", "Una vez a la semana", "Entre 2 y 4 veces a la semana", "Al menos 5 veces a la semana")
valores_interes <- factor(valores_interes, levels = valores_interes)

# Inicializar un vector para almacenar los conteos
conteos <- sapply(valores_interes, function(valor) sum(datos$`¿Con qué frecuencia el Municipio recolecta los residuos en sus inmediaciones?` == valor))

# Convertir el resultado a un data frame
conteo_df <- data.frame(valores = valores_interes, Frecuencia = conteos)

# Crear la gráfica de barras
ggplot(conteo_df, aes(x = valores, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", size = 0.5) +
  theme_bw() +
  labs(title = "Frecuencia de tiempo de recoleccion de basura", 
       x = "Recolecciones por semana",
       y = "Frecuencia") +
  theme(text = element_text(size = 14, family = "Trebuchet MS"))

ggsave("Frecuencia de recoleccion de residuos.png", last_plot(), dpi=600)

###3 Tipo de desagüe###

# Especificar los valores de interés
valores_interes <- c("Desagüe a red cloacal pública", "A pozo negro/ciego", "Desagüe a red cloacal informal/vecinal", "A cámara séptica", "No sabe")

# Inicializar un vector para almacenar los conteos
conteos <- sapply(valores_interes, function(valor) sum(datos$`¿Qué tipo de desagüe posee la vivienda?` == valor))

# Convertir el resultado a un data frame
conteo_df <- data.frame(Variable = valores_interes, Frecuencia = conteos)

# Calcular porcentajes
conteo_df$Porcentaje <- round((conteo_df$Frecuencia / sum(conteo_df$Frecuencia)) * 100, 1)

conteo_df$VariableConPorcentajes <- paste0(conteo_df$Variable, ' [ ', (conteo_df$Porcentaje), '% ]')

# Crear la gráfica de torta
ggplot(conteo_df, aes(x = "", y = Frecuencia, fill = VariableConPorcentajes)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Porcentaje de presencia de cada desagüe en casas con plagas") +
  guides(fill=guide_legend(title="")) +
  theme(text = element_text(size = 14, family = "Trebuchet MS"))

ggsave("Tipo de desague.png", last_plot(), dpi=600)

###5 Presencia de plagas en casas###

 
#Incializo el vector que almacena los conteos de casas con plagas vs sin plagas
conteos <- sapply(c("No", "Sí"), function(valor) sum(datos$`¿Hay plagas (cucarachas, mosquitos, ratas, etc) en su vivienda y en los alrededores de la misma?` == valor))

# Convertir el resultado a un data frame
conteo_df <- data.frame(Variable = c("No", "Sí"), Frecuencia = conteos)

# Calcular porcentajes
conteo_df$Porcentaje <- round((conteo_df$Frecuencia / sum(conteo_df$Frecuencia)) * 100, 1)

# Crear la gráfica de torta
ggplot(conteo_df, aes(x = "", y = Frecuencia, fill = Variable)) +
  geom_bar(stat = "identity", width = 1, color = "black", size = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Preséncia de plagas en las casas") +
  theme(legend.position = "") +
  theme(text = element_text(family = "Trebuchet MS")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = paste0(Variable)), 
            position = position_stack(vjust = 0.5),
            size = 6) +
  scale_fill_brewer(palette="Set1")

ggsave("Plagas.png", last_plot(), dpi=600)

###6 Tipos de plagas en casas con plagas###

# Calcular el porcentaje de viviendas con cucarachas
porcentaje_cucarachas <- sum(datos$`Cucarachas?` == "Cucarachas") / nrow(datos) * 100

# Calcular el porcentaje de viviendas con mosquitos
porcentaje_mosquitos <- sum(datos$`Mosquitos?` == "Mosquitos") / nrow(datos) * 100

#caluclo el porcentaje de viviendas con ratas
porcentaje_ratas <- sum(datos$`Ratas?` == "Ratas") / nrow(datos) * 100

# Crear un data frame con los resultados
resultados <- data.frame(
  Tipo = c("Cucarachas", "Mosquitos", "Ratas"),
  Porcentaje = c(porcentaje_cucarachas, porcentaje_mosquitos, porcentaje_ratas)
)


# Crear la gráfica de barras del porcentaje de presencia de cada plaga
ggplot(resultados, aes(x = Tipo, y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity",color = "black", size = 1, fill = "skyblue") +
  theme_bw() +
  labs(title = "Porcentaje de Viviendas con Plagas",
       x = "Tipo de Plaga",
       y = "Porcentaje") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), vjust = -0.5) +
  ylim(0, 100)

ggsave("Tipos de plagas.png", last_plot(), dpi=600)


###7 Cantidad de personas por vivienda###

# Crear la gráfica de bastones
ggplot(datos) +
  aes(x = `¿Cuántos integrantes hay en su vivienda?`) + 
  geom_bar(width = 0.10) +
  scale_x_continuous(n.breaks = 10) +
  labs(x = "Número de personas en la misma vivienda",
       y = "Número de viviendas") +
  theme_bw()

ggsave("Cantidad de personas por vivienda.png", last_plot(), dpi=600)


###8 Cant máxima de personas por habitación###

ggplot(datos) +
  aes(x = `¿Cuál es el número MÁXIMO de personas que duermen en estos dormitorios usualmente?`) + 
  geom_bar(width = 0.10) +
  scale_x_continuous(n.breaks = 10) +
  labs(x = "Personas durmiendo en el mismo dormitorio",
       y = "Número de viviendas") +
  theme_bw()

ggsave("Personas durmiendo por dormitorio.png", last_plot(), dpi=600)

###7 Precio de alquiler###


#Incializo el vector que almacena los conteos de casas con plagas vs sin plagas
datosAlquiler <- datos[datos$`El lugar que habitan actualmente es:` == 'Alquilado', ]

# Crear el histograma
ggplot(datosAlquiler, aes(x = `¿Cuál es el costo actual del mismo?`)) +
  geom_histogram(fill = "skyblue", col = "black", 
                 breaks = seq(0, 25000, 5000)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 25000, 5000), labels = scales::unit_format(unit = "$")) +
  labs(x = "Precio", y = "Cantidad de viviendas")

ggsave("Precio del alquiler.png", last_plot(), dpi=600)


