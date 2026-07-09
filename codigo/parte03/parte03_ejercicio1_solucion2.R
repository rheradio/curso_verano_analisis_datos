# Este script crea un gráfico de barras vertical con los precios del Big Mac por continente a partir de un fichero .csv que contiene los precios del Big Mac por país. 

# Leer fichero "precio_bigmac_por_pais.csv"
bigmac_data <- read.csv("precio_bigmac_por_pais.csv")

# mostrar los 10 primeros registros del dataframe
head(bigmac_data, n = 10)

# crear una columna nueva con los precios en euros a parti de la columna dollar_price
bigmac_data$euro_price <- bigmac_data$dollar_price * 0.87

# Crear una nueva columna con el continente del pais indicado en la columna name
bigmac_data$continent <- countrycode(bigmac_data$name, origin = "country.name", destination = "continent", custom_match = c("Euro area" = "Europe"))

head(bigmac_data, n = 10)

# Crear un nuevo dataframe con el precio medio del Big Mac por continente a partir de la columna euro_price y la columna continent
mean_prices <- aggregate(euro_price ~ continent, data = bigmac_data, FUN = mean)

# Reordenar precios medios de mayor a menor
mean_prices <- mean_prices[order(mean_prices$euro_price, decreasing = TRUE), ]


# Mostrar un grafico de barras con los precios medios del Big Mac por continente, ordenado de mayor a menor
barplot(mean_prices$euro_price, names.arg = mean_prices$continent, main = "Precio Medio del Big Mac por Continente", xlab = "Continente", ylab = "Precio en Euros", las = 2)
# Poner colores a las barras (rojo y verde NO pueden ir juntos)
grafico <- barplot(mean_prices$euro_price, names.arg = mean_prices$continent, main = "Precio Medio del Big Mac por Continente", xlab = "Continente", ylab = "Precio en Euros", las = 2, col = c("red", "blue", "green", "yellow", "purple"))

# Guardar el grafico en .pdf
dev.copy(pdf, "precio_bigmac_por_continente.pdf")
dev.off()