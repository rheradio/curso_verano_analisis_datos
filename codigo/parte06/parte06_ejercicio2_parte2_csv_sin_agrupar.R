# Instala si no tienes
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("mapSpain")

library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(mapSpain)

# === 1. Cargar datos del CSV ===
# Reemplaza la ruta por el fichero real
datos <- read_csv("estaciones.csv")  # por ejemplo: "datos_estaciones.csv"

# === 2. Convertir a objeto espacial ===
estaciones_sf <- st_as_sf(datos, coords = c("longitud", "latitud"), crs = 4326)

# === 3. Obtener mapa base de provincias ===
provincias <- esp_get_prov(moveCAN = FALSE)

# === 4. Crear el mapa ===
ggplot() +
  geom_sf(data = provincias, fill = "grey95", color = "white") +
  geom_sf(data = estaciones_sf, aes(color = provincia), size = 3, alpha = 0.7) +
  scale_color_viridis_d(option = "C") +
  labs(
    title = "Estaciones de tren por provincia en España",
    subtitle = "Posicionamiento geográfico basado en latitud y longitud",
    color = "Provincia"
  ) +
  theme_minimal()
