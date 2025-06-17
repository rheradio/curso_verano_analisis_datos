# Lee un fichero .csv
data <- read.csv("precio_bigmac_por_pais.csv")
# A partir de la columna dollar_price crea una nueva columna con el precio en euros
data$euro_price <- data$dollar_price * 0.93
# Filtrar los países europeos
european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                        "Czech Republic", "Denmark", "Estonia", "Finland", 
                        "France", "Germany", "Greece", "Hungary", "Ireland", 
                        "Italy", "Latvia", "Lithuania", "Luxembourg", 
                        "Malta", "Netherlands", "Poland", "Portugal", 
                        "Romania", "Slovakia", "Slovenia", "Spain", 
                        "Sweden")
# Filtrar el dataset por países europeos
european_data <- data[data$name %in% european_countries, ]
# Filtrar los países asiaticos
asian_countries <- c("China", "Hong Kong", "India", "Indonesia", "Japan", 
                      "Malaysia", "Pakistan", "Philippines", "Singapore", 
                      "South Korea", "Taiwan", "Thailand", "Vietnam")
# Filtrar el dataset por países asiáticos
asian_data <- data[data$name %in% asian_countries, ]
# Filtrar los países americanos
american_countries <- c("Argentina", "Brazil", "Canada", "Chile", "Colombia", 
                         "Mexico", "Peru", "United States")
# Filtrar el dataset por países americanos
american_data <- data[data$name %in% american_countries, ]
# Filtrar el dataset por países africanos
african_countries <- c("Algeria", "Egypt", "Morocco", "Nigeria", "South Africa")
# Filtrar el dataset por países africanos
african_data <- data[data$name %in% african_countries, ]
# Filtrar el dataset por países oceánicos
oceania_countries <- c("Australia", "New Zealand")
# Filtrar el dataset por países oceánicos
oceania_data <- data[data$name %in% oceania_countries, ]


# A partir de los datos european_data, asian_data, and american_data, Mostrar un gráfico de barras vertical con los precios del bigmac por continente
# debe aparecer ordenador de menor a mayor.
library(ggplot2)
# Crear un dataframe combinado con los datos de los tres continentes
combined_data <- rbind(
  data.frame(Continent = "Europe", Price = european_data$euro_price),
  data.frame(Continent = "Asia", Price = asian_data$euro_price),
  data.frame(Continent = "America", Price = american_data$euro_price),
  data.frame(Continent = "Africa", Price = african_data$euro_price),
  data.frame(Continent = "Oceania", Price = oceania_data$euro_price)
)
# Calcular la media de los precios por continente
mean_prices <- aggregate(Price ~ Continent, data = combined_data, FUN = mean)
# Ordenar los datos por precio medio
mean_prices <- mean_prices[order(mean_prices$Price), ]
# Crear el gráfico de barras vertical ordenado de mayor a menor y cada continente de un color
ggplot(mean_prices, aes(x = reorder(Continent, -Price), y = Price, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Precio medio del Big Mac por continente", x = "Continente", y = "Precio medio (EUR)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")
#Añade el valor exacto en cada barra
ggplot(mean_prices, aes(x = reorder(Continent, -Price), y = Price, fill = Continent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Price, 2)), vjust = -0.5) +  # Añadir etiquetas con el valor exacto
  labs(title = "Precio medio del Big Mac por continente", x = "Continente", y = "Precio medio (EUR)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")
# Exporta el gráfico a .pdf
ggsave("bigmac_prices_by_continent.pdf", width = 8, height = 6)
