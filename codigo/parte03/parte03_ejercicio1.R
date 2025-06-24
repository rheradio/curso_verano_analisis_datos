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
                        "Sweden", "Ukraine", "United Kingdom", "Iceland", "Norway", "Switzerland", "Liechtenstein", "Andorra", "Monaco", "San Marino", "Vatican City", "Albania", "Bosnia and Herzegovina", "North Macedonia", "Serbia", "Montenegro", "Kosovo", "Moldova", "Belarus", "Russia", "Armenia", "Azerbaijan", "Georgia", "Turkey", "Cyprus", "Britain")
# Filtrar el dataset por países europeos
european_data <- data[data$name %in% european_countries, ]
# Filtrar los países asiaticos
asian_countries <- c("China", "Hong Kong", "India", "Indonesia", "Japan", 
                      "Malaysia", "Pakistan", "Philippines", "Singapore", 
                      "South Korea", "Taiwan", "Thailand", "Vietnam", "United Arab Emirates", "Sri Lanka", "Bangladesh", "Nepal", "Cambodia", "Myanmar", "Brunei", "Mongolia", "Kazakhstan", "Uzbekistan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Laos", "Timor-Leste", "Bhutan", "Kuwait", "Jordan", "Qatar", "Oman", "Bahrain", "Iraq", "Syria", "Yemen", "Afghanistan", "Armenia", "Azerbaijan", "Georgia", "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan", "Lebanon")
# Filtrar el dataset por países asiáticos
asian_data <- data[data$name %in% asian_countries, ]
# Filtrar los países americanos
american_countries <- c("Argentina", "Brazil", "Canada", "Chile", "Colombia", 
                         "Mexico", "Peru", "United States", "Uruguay", "Venezuela", "Costa Rica", "Guatemala", "Panama", "Puerto Rico", "Dominican Republic", "Honduras", "Nicaragua", "El Salvador", "Bolivia", "Ecuador", "Paraguay", "Cuba", "Jamaica", "Trinidad and Tobago", "Barbados", "Bahamas", "Belize", "Saint Lucia", "Saint Vincent and the Grenadines", "Grenada", "Antigua and Barbuda", "Dominica", "Saint Kitts and Nevis", "Suriname", "Guyana", "French Guiana", "Bermuda", "Greenland", "Saint Pierre and Miquelon", "Falkland Islands", "Aruba", "Curacao", "Sint Maarten", "British Virgin Islands", "U.S. Virgin Islands", "Cayman Islands", "Turks and Caicos Islands")
# Filtrar el dataset por países americanos
american_data <- data[data$name %in% american_countries, ]
# Filtrar el dataset por países africanos
african_countries <- c("Algeria", "Egypt", "Morocco", "Nigeria", "South Africa", 
                       "Tunisia", "Kenya", "Ghana", "Senegal", "Tanzania", 
                       "Uganda", "Zimbabwe", "Cameroon", "Cote d'Ivoire", 
                       "Mali", "Rwanda", "Zambia", "Angola", "Botswana", 
                       "Namibia", "Sierra Leone", "South Sudan", "Ethiopia")
# Filtrar el dataset por países africanos
african_data <- data[data$name %in% african_countries, ]
# Filtrar el dataset por países oceánicos
oceania_countries <- c("Australia", "New Zealand", "Fiji", "Papua New Guinea", 
                       "Samoa", "Solomon Islands", "Tonga", "Vanuatu", 
                       "Kiribati", "Marshall Islands", "Micronesia", 
                       "Nauru", "Palau", "Tuvalu", "Cook Islands", 
                       "Niue", "French Polynesia", "New Caledonia", 
                       "Wallis and Futuna", "American Samoa")
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
# Q: cuantos datos tiene cada continente?
# Contar el número de datos por continente
continent_counts <- aggregate(Price ~ Continent, data = combined_data, FUN = length)
# Mostrar el número de datos por continente
print(continent_counts)
# Q: Cuantos datos hay en total?
total_data_count <- nrow(combined_data)
print(total_data_count)
# Q: ¿Cuál es el precio medio del Big Mac en Europa?
mean_europe_price <- mean(european_data$euro_price, na.rm = TRUE)
# Mostrar el precio medio del Big Mac en Europa
print(paste("Precio medio del Big Mac en Europa:", round(mean_europe_price, 2), "EUR"))
# Q: muestra los paises que estan en el dataset original y no están en el american_data, asian_data, african_data, european_data, oceania_data

# Crea un gráfico de tartas con el porcentaje de países por continente del dataset original
# Calcular el número de países por continente
continent_counts <- data.frame(
  Continent = c("Europe", "Asia", "America", "Africa", "Oceania"),
  Count = c(nrow(european_data), nrow(asian_data), nrow(american_data), nrow(african_data), nrow(oceania_data))
)
# Crear el gráfico de tartas
ggplot(continent_counts, aes(x = "", y = Count, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Porcentaje de países por continente", x = "", y = "") +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "right")

# Crea un gráfico de tartas con el porcentaje de países por continente del dataset original mostrando el dato por cada porción

# Calcular el número de países por continente
continent_counts <- data.frame(
  Continent = c("Europe", "Asia", "America", "Africa", "Oceania"),
  Count = c(nrow(european_data), nrow(asian_data), nrow(american_data), nrow(african_data), nrow(oceania_data))
)
# Crear el gráfico de tartas
ggplot(continent_counts, aes(x = "", y = Count, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Porcentaje de países por continente", x = "", y = "") +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(round(Count / sum(Count) * 100, 1), "%")), position = position_stack(vjust = 0.5))
# Convierte el gráfico de tarta en un gráfico de barras
# Crear el gráfico de barras con el porcentaje de países por continente
ggplot(continent_counts, aes(x = Continent, y = Count, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de países por continente", x = "Continente", y = "Número de países") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = Count), vjust = -0.5)  # Añadir etiquetas con el número de países

