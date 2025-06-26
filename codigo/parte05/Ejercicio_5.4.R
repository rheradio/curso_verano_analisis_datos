# Cargar datos
datos <- read.csv("Life_Expectancy_Data.csv")

#Crear columna de continente a partir del país
datos$Continent <- countrycode(sourcevar = datos$Country,
                               origin = "country.name",
                               destination = "continent")

# Revisar países no reconocidos
unique(datos$Country[is.na(datos$Continent)])

# Filtrar año 2014 y eliminar filas con NA en las variables relevantes
datos2014 <- datos[datos$Year == 2014 & 
                     !is.na(datos$Continent) & 
                     !is.na(datos$Alcohol) & 
                     !is.na(datos$Life.expectancy), ]

# Crear terciles del consumo de alcohol
alcohol_terciles <- quantile(datos2014$Alcohol, probs = c(1/3, 2/3), na.rm = TRUE)

# Crear variable categórica de consumo: bajo, medio, alto
datos2014$Alcohol_cat <- cut(datos2014$Alcohol,
                             breaks = c(-Inf, alcohol_terciles[1], alcohol_terciles[2], Inf),
                             labels = c("Bajo", "Medio", "Alto"),
                             right = TRUE)

# Verificar distribución
table(datos2014$Alcohol_cat, datos2014$Continent)

# Modelo
fact.aov.model <- aov(Life.expectancy ~ Continent * Alcohol_cat,
                      data = datos2014)

# Normalidad de los residuos
fact.aov.residuals <- residuals(object = fact.aov.model)
shapiro.test(fact.aov.residuals)

# Homogeneidad de la varianza
leveneTest(datos2014$Life.expectancy ~ datos2014$Continent *
             datos2014$Alcohol_cat)

# ANOVA
summary(fact.aov.model)

# creamos el modelo lineal que subyace al ANOVA
datos2014$Continent <- factor(datos2014$Continent)
datos2014$Alcohol_cat <- factor(datos2014$Alcohol_cat)
lm.model <- lm(Life.expectancy ~ Continent * Alcohol_cat,
               data=datos2014)
# analizamos los efectos simples
testInteractions(lm.model, fixed="Continent",
                 across="Alcohol_cat")
