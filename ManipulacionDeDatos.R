install.packages("dplyr")

library(dplyr)

# Tipo de desagüe

# Fijo el dataset
attach(datos)

conditions <- c("Desagüe a red cloacal pública",
                "A pozo negro/ciego",
                "Desagüe a red cloacal informal/vecinal",
                "No sabe")

replacement_values <- c("CP", "PN", "CI", "NS")

datos$`¿Qué tipo de desagüe posee la vivienda?` <- 
  replace(datos$`¿Qué tipo de desagüe posee la vivienda?`, 
          datos$`¿Qué tipo de desagüe posee la vivienda?` %in% conditions, 
          replacement_values)

conditions <- c("Sí, a menos de 500 metros",
                "Sí, a más de 500 metros y menos de 2 kilómetros")

replacement_values <- c("500", "500-2000")

datos$`¿Hay basurales cerca de su vivienda?` <- 
  replace(datos$`¿Hay basurales cerca de su vivienda?`, 
          datos$`¿Hay basurales cerca de su vivienda?` %in% conditions, 
          replacement_values)


names(datos)[92] = "Cucarachas?"
names(datos)[93] = "Mosquitos?"
names(datos)[94] = "Ratas?"
