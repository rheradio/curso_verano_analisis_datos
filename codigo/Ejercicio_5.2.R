# Cargar datos
life.expectancy.data <- read.csv("Life_Expectancy_Data.csv")

# Filtrar los años 2000 y 2015
data_2015 <- subset(life.expectancy.data, Year == 2015)

# Asegurarse de eliminar NA
life_2015 <- na.omit(data_2015[, c("Life.expectancy", "Status")])
life_2015$Status <- factor(life_2015$Status)

#Test de Levene
library(car)
levene_result <- leveneTest(Life.expectancy ~ Status, data = life_2015)

#T-test
t_test_result <- t.test(Life.expectancy ~ Status, data = life_2015, var.equal = FALSE)

#Tamaño del efecto
library(effectsize)
cohens_d(Life.expectancy ~ Status, data = life_2015, pooled_sd = T)
