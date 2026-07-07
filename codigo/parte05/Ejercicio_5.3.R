library(countrycode)
#Cargar el dataset
datos <- read.csv("Life_Expectancy_Data.csv")

#Crear columna de continente a partir del país
datos$Continent <- countrycode(sourcevar = datos$Country,
                               origin = "country.name",
                               destination = "continent")

# Revisar países no reconocidos
unique(datos$Country[is.na(datos$Continent)])

# Filtrar para el año 2015
datos2015 <- datos[datos$Year == 2015 & !is.na(datos$Life.expectancy), ]

# Modelo
aov.model <- aov(datos2015$Life.expectancy ~ datos2015$Continent)

# Normalidad por grupo (Shapiro-Wilk test)
aov.residuals <- residuals(object = aov.model)
shapiro.test(aov.residuals)

# n de cada grupo
table(datos2015$Continent)

# Homogeneidad de varianzas (Levene's Test)
leveneTest(Life.expectancy ~ Continent, data = datos2015)

# ANOVA
summary(aov.model)

# Tamaño del efecto
etaSquared(aov.model)

# Prueba post-hoc (Tukey)
TukeyHSD(aov.model)