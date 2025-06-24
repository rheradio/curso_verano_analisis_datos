install.packages("mapSpain")  # Si no lo tienes
library(mapSpain)
library(ggplot2)
library(dplyr)

# Obtener geometría provincial
mapa <- esp_get_prov()

# Tus datos (adaptado a formato compatible con el shapefile)
datos <- tibble::tibble(
  ine.prov.name = c("Tarragona", "Zaragoza", "León", "A Coruña", "Badajoz", "Pontevedra", "Palencia", "Teruel", 
                    "Girona", "Huesca", "Toledo", "Ciudad Real", "Huelva", "Ourense", "Lugo", "Lleida", 
                    "Valladolid", "Navarra", "Málaga", "Salamanca", "Cuenca", "Valencia", "Alicante", "Ávila", 
                    "Cáceres", "Zamora", "Guadalajara", "Segovia", "Albacete", "Jaén", "Soria", "Cádiz", 
                    "Castellón", "Córdoba", "Madrid", "La Rioja", "Sevilla", "Álava", "Burgos", "Granada", 
                    "Murcia", "Almería", "Barcelona", "Asturias"),
  estaciones = c(45, 38, 30, 29, 28, 25, 23, 22, 21, 20, 19, 19, 19, 18, 17, 17,
                 16, 16, 16, 15, 14, 14, 13, 12, 12, 12, 12, 11, 11, 10, 10, 10, 
                 10, 10, 10, 10, 10, 9, 8, 8, 7, 6, 5, 1)
)

# Unir shapefile con datos
mapa_datos <- mapa %>%
  left_join(datos, by = "ine.prov.name")

# Graficar
ggplot(mapa_datos) +
  geom_sf(aes(fill = estaciones), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Número de estaciones de tren por provincia",
       fill = "Estaciones") +
  theme_minimal()
