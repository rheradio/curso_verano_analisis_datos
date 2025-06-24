# Instalar paquetes si es necesario
#install.packages(c("ggplot2", "sf", "dplyr", "readr", "ggrepel", "rnaturalearthhires", "rnaturalearth", "rnaturalearthdata"))
#install.packages("devtools")
devtools::install_github("ropensci/rnaturalearthhires")
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)

# Cargar geometría de provincias de España
spain_map <- ne_states(country = "Spain", returnclass = "sf")

# Crear tabla de frecuencias manualmente (basada en gráfico)
stations_data <- tibble::tibble(
  name = c("Tarragona", "Zaragoza", "León", "A Coruña", "Badajoz", "Pontevedra", "Palencia", "Teruel", 
           "Girona", "Huesca", "Toledo", "Ciudad Real", "Huelva", "Ourense", "Lugo", "Lleida", 
           "Valladolid", "Navarra", "Málaga", "Salamanca", "Cuenca", "Valencia", "Alicante", "Ávila", 
           "Cáceres", "Zamora", "Guadalajara", "Segovia", "Albacete", "Jaén", "Soria", "Cádiz", 
           "Castellón", "Córdoba", "Madrid", "La Rioja", "Sevilla", "Álava", "Burgos", "Granada", 
           "Murcia", "Almería", "Barcelona", "Asturias"),
  estaciones = c(45, 38, 30, 29, 28, 25, 23, 22, 21, 20, 19, 19, 19, 18, 17, 17,
                 16, 16, 16, 15, 14, 14, 13, 12, 12, 12, 12, 11, 11, 10, 10, 10, 
                 10, 10, 10, 10, 10, 9, 8, 8, 7, 6, 5, 1)
)

# Corregir nombres si es necesario para hacer join
stations_data <- stations_data %>%
  mutate(name = case_when(
    name == "A Coruña" ~ "Galicia",  # según cómo venga el shapefile
    name == "Álava" ~ "Basque Country",
    TRUE ~ name
  ))

# Unir mapa con datos
spain_map_data <- spain_map %>%
  left_join(stations_data, by = "name")

# Calcular centroides para ubicar las burbujas
spain_map_data <- spain_map_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])

# Graficar mapa de burbujas
ggplot(data = spain_map_data) +
  geom_sf(fill = "gray95", color = "white") +
  geom_point(aes(x = lon, y = lat, size = estaciones), color = "steelblue", alpha = 0.7) +
  scale_size_continuous(range = c(2, 15)) +
  ggrepel::geom_text_repel(
    aes(x = lon, y = lat, label = name),
    size = 3,
    max.overlaps = 30
  ) +
  labs(title = "Estaciones de tren por provincia en España",
       subtitle = "Tamaño de la burbuja proporcional al número de estaciones",
       size = "Nº de estaciones") +
  theme_minimal()
