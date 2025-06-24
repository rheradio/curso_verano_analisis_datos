## Leer el CSV y procesar los datos
# Paquetes necesarios
library(tidyverse)

# Leer el CSV
datos <- read.csv("euromillones.csv", header = FALSE)

# Renombrar columnas
colnames(datos) <- c("Fecha", "N1", "N2", "N3", "N4", "N5", "Vacio", "E1", "E2")

# Convertir columnas numéricas
numeros <- datos %>% select(N1:N5) %>% unlist() %>% as.integer()
estrellas <- datos %>% select(E1:E2) %>% unlist() %>% as.integer()

##################################################
## Visualización de las frecuencias
# Crear data frame de frecuencias
frecuencia_numeros <- as.data.frame(table(numeros))
frecuencia_estrellas <- as.data.frame(table(estrellas))

# Convertir a número
frecuencia_numeros$numeros <- as.integer(as.character(frecuencia_numeros$numeros))
frecuencia_estrellas$estrellas <- as.integer(as.character(frecuencia_estrellas$estrellas))

# Gráfico de barras para los números
ggplot(frecuencia_numeros, aes(x = numeros, y = Freq)) +
  geom_col(fill = "steelblue") +
  labs(title = "Frecuencia de Números en Euromillones",
       x = "Número", y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras para las estrellas
ggplot(frecuencia_estrellas, aes(x = estrellas, y = Freq)) +
  geom_col(fill = "darkorange") +
  labs(title = "Frecuencia de Estrellas en Euromillones",
       x = "Estrella", y = "Frecuencia") +
  theme_minimal()

##################################################
## Test de chi-cuadrado de bondad de ajuste
# Frecuencia observada
observadas_numeros <- table(factor(numeros, levels = 1:50))

# Frecuencia esperada: total sorteos × 5 números por sorteo / 50 números
frecuencia_esperada <- rep(length(numeros)/50, 50)

# Test Chi-cuadrado
chisq.test(observadas_numeros, p = rep(1/50, 50))

# Frecuencia observada
observadas_estrellas <- table(factor(estrellas, levels = 1:12))

# Test Chi-cuadrado
chisq.test(observadas_estrellas, p = rep(1/12, 12))

##################################################
## Visualiza qué estrellas están desviadas
ggplot(frecuencia_estrellas, aes(x = factor(estrellas), y = Freq)) +
  geom_col(fill = "darkorange") +
  geom_hline(yintercept = mean(frecuencia_estrellas$Freq), linetype = "dashed", color = "red") +
  labs(title = "Frecuencia de Estrellas vs Esperado",
       subtitle = paste("Media esperada =", round(mean(frecuencia_estrellas$Freq), 2)),
       x = "Estrella", y = "Frecuencia") +
  theme_minimal()


##################################################
## Calcula residuos para ver qué estrellas desvían más
test_estrellas <- chisq.test(observadas_estrellas, p = rep(1/12, 12))
test_estrellas$stdres  # Residuos tipificados

##################################################

