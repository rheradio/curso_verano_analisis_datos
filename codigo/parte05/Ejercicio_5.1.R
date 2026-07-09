# Cargar datos
life.expectancy.data <- read.csv("Life_Expectancy_Data.csv")

# Filtrar los años 2000 y 2015
data_2000 <- subset(life.expectancy.data, Year == 2000)
data_2015 <- subset(life.expectancy.data, Year == 2015)

# Asegurarse de eliminar NA
life_2000 <- na.omit(data_2000$Life.expectancy)
life_2015 <- na.omit(data_2015$Life.expectancy)

# T test
t_result <- t.test(life_2000, life_2015, paired=TRUE)

# Tamaño del efecto
library(lsr)
cohens_d(life_2000, life_2015, paired = T)