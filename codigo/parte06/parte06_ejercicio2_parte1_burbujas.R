# Instalar si no los tienes
install.packages("mapSpain")
install.packages("ggplot2")
install.packages("dplyr")

library(mapSpain)
library(ggplot2)
library(dplyr)

# Obtener geometría de provincias
mapa <- esp_get_prov()

# Tus datos de estaciones por provincia
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

# Unir geometría y datos
mapa_datos <- mapa %>%
  left_join(datos, by = "ine.prov.name")

# Centroides para burbujas
centroides <- st_centroid(mapa_datos)

# Categoría por número de estaciones
centroides <- centroides %>%
  mutate(rango = cut(estaciones,
                     breaks = c(-Inf, 10, 20, 30, 40, Inf),
                     labels = c("≤10", "11–20", "21–30", "31–40", ">40")))

# Mapa con burbujas coloreadas
ggplot() +
  geom_sf(data = mapa, fill = "grey95", color = "white") +
  geom_sf(data = centroides, aes(size = estaciones, color = rango), alpha = 0.7) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = c("≤10" = "steelblue", 
                                "11–20" = "forestgreen", 
                                "21–30" = "orange", 
                                "31–40" = "darkred", 
                                ">40" = "purple")) +
  labs(title = "Número de estaciones de tren por provincia (España)",
       subtitle = "Mapa con burbujas de color por rango de estaciones",
       size = "Nº estaciones",
       color = "Rango") +
  theme_minimal()
ggsave("mapa_estaciones.pdf", width = 8, height = 6)