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

