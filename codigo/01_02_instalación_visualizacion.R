# install.packages("tidyverse", "ggrepel", "palmerpenguins", "ggridges")

library(tidyverse)

# Variación ---------------------------------------------------------------

## 1 Variable numérica --------------------------------------------------

### Galton ------------------------------------------------------------------

galton <- read_csv("datos/galton.csv")

# Rango

galton |>
  summarize(
    min = min(child),
    max = max(child)
  )

# Percentiles al 95%

galton |>
  summarize(
    inf = quantile(child, 0.25 / 2),
    sup = quantile(child, 1 - 0.25 / 2),
    mediana_metodo_1 = quantile(child, 0.5),
    mediana_metodo_2 = median(child)
  )

# Histograma

galton |>
  ggplot(aes(x = child)) +
  geom_density(fill = "lightblue")

# Media y varianza

galton |>
  summarize(
    media = mean(child),
    varianza = var(child),
    desviacion = sd(child)
  )

## Brevísima intro a ggplot2 ----------------------------------------------------------

james_bond <- read_csv("datos/james_bond.csv")

ggplot(data = james_bond)

james_bond |>
  ggplot()

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond))

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) + # OJO es + en vez de |>
  geom_point()

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) + # OJO es + en vez de |>
  geom_point(size = 3)

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) +
  geom_point(aes(size = Kills_Bond))

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) + # OJO es + en vez de |>
  geom_line()

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) +
  geom_point() +
  geom_smooth()

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm)

ggsave("figuras/james_bond.png", width = 5, height = 4)

### Número de parejas  ------------------------------------------------------------------

parejas <- read_csv("datos/numero_de_parejas_por_sexo.csv")

# Histograma
# Prueba a cambiar el nº de bars. Prueba con binwidth o con bins

parejas |>
  filter(sexo == "Hombre") |>
  ggplot(aes(x = numero_de_parejas)) +
  geom_histogram()

# Gráfica de densidad

parejas |>
  filter(sexo == "Hombre") |>
  ggplot(aes(x = numero_de_parejas)) +
  geom_density(fill = "lightblue")

# Los valores atípicos (outliers) ofuscan la visualización

# Método 1: quitamos los valores atípicos
# Ojo con borrar los outliers, nos podemos quedar sin datos!

parejas |>
  filter(sexo == "Hombre") |>
  filter(numero_de_parejas < 30) |>
  ggplot(aes(x = numero_de_parejas)) +
  geom_density(fill = "lightblue") +
  scale_x_continuous(breaks = seq(0, 30, 2))

# Método 2: hacemos zoom

parejas |>
  filter(sexo == "Hombre") |>
  ggplot(aes(x = numero_de_parejas)) +
  geom_density(fill = "lightblue") +
  coord_cartesian(xlim = c(0, 30)) +
  scale_x_continuous(breaks = seq(0, 30, 2))

## 1 variable categórica ---------------------------------------------------

### James Bond --------------------------------------------------------------

# Gráfica de barras

james_bond |>
  ggplot(aes(x = Bond)) +
  geom_bar()

# intercambiamos los ejes

james_bond |>
  ggplot(aes(y = Bond)) +
  geom_bar()

# Ordenamos las barras

james_bond |>
  ggplot(aes(y = fct_infreq(Bond))) +
  geom_bar(aes(fill = Bond), color = "black")

# Diagramas de tarta

james_bond |>
  ggplot(aes(y = factor(1), fill = Bond)) +
  geom_bar(aes(fill = Bond), width = 1, color = "black") +
  coord_polar() +
  theme(
    line = element_blank(),
    text = element_blank(),
    title = element_blank()
  )

### EJERCICIO Estaciones Tren (Media y Larga Distancia) -------------------------------

# NO HAGAS TRAMPA !!!!!!!!!!!!!!!!!!!!!!!!

# estaciones_tren <- read_csv("datos/estaciones_tren_larga_y_media_distancia.csv")
#
# estaciones_tren |>
#   ggplot(aes(y = fct_infreq(provincia))) +
#   geom_bar()
#
# ggsave("figuras/estaciones_tren.pdf", width = 5, height = 6)


# Covariación -------------------------------------------------------------

## Numérica + categórica -----------------------------------

# Una gráfica de densidad por cada categoría
parejas |>
  ggplot(aes(x = numero_de_parejas, color = sexo)) +
  geom_density(linewidth = 1, alpha = 0.2)

# Intentamos minimizar el solapamiento
library(ggridges)

parejas |>
  ggplot(aes(x = numero_de_parejas, y = sexo, color = sexo, fill = sexo)) +
  geom_density_ridges(alpha = 0.2)

# Mejor un boxplot
parejas |>
  ggplot(aes(x = numero_de_parejas, y = sexo)) +
  geom_boxplot()

# Mejor aún un boxplot sin los outliers
parejas |>
  ggplot(aes(y = numero_de_parejas, x = sexo)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 50))

## 2 variables numéricas ---------------------------------------------------

### James Bond --------------------------------------------------------------

james_bond |>
  ggplot(aes(x = Year, y = Martinis, color = Bond, shape = Bond)) +
  geom_point()

### EJERCICIO James Bond -------------------------------

# NO HAGAS TRAMPA !!!!!!!!!!!!!!!!!!!!!!!!

# james_bond |>
#   ggplot(aes(x = Budget, y = Avg_User_IMDB)) +
#   geom_point()
#
# james_bond |>
#   ggplot(aes(x = Budget, y = Avg_User_IMDB)) +
#   geom_point(aes(color = Year))
#
# james_bond |>
#   mutate(last_25_years = if_else(Year < 2000, FALSE, TRUE)) |>
#   ggplot(aes(x = Budget, y = Avg_User_IMDB)) +
#   geom_point(aes(color = last_25_years))
#
# james_bond |>
#   mutate(last_25_years = if_else(Year < 2000, FALSE, TRUE)) |>
#   ggplot(aes(x = Budget, y = Avg_User_IMDB, color = last_25_years)) +
#   geom_point() +
#   geom_smooth(se = FALSE, method = "lm")

### Overplotting (pingüinos) ---------------------------------------------------------------------

library(palmerpenguins)

penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

### Desigualdad (INE) ---------------------------------------------------------------------

desigualdad <- read_csv("datos/ine_desigualdad_comunidad_autonoma.csv")

desigualdad |>
  ggplot(aes(x = anno, y = Gini)) +
  geom_point()

desigualdad |>
  ggplot(aes(x = anno, y = Gini, color = comunidades_y_ciudades_autonomas)) +
  geom_line()

desigualdad |>
  ggplot(aes(x = anno, y = Gini)) +
  geom_line() +
  facet_wrap(~comunidades_y_ciudades_autonomas, ncol = 4)

### Desempleo (INE) ---------------------------------------------------------------------

desempleo_estudios <- read_csv("datos/ine_desempleo_edad_nivel_estudios.csv")

desempleo_estudios |>
  ggplot(aes(x = anno, porcentaje, color = grupo_de_edad)) +
  geom_point() +
  geom_line() +
  facet_wrap(~nivel_de_formacion_alcanzado) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 5))

niveles_estudios <- c(
  "Menos que primaria",
  "Ed. primaria y primera etapa de Ed. secundaria",
  "Segunda etapa de Ed. secundaria y Ed. postsecundaria no superior",
  "Ed. superior"
)

desempleo_estudios <- desempleo_estudios |>
  mutate(
    nivel_de_formacion_alcanzado =
      factor(nivel_de_formacion_alcanzado, levels = niveles_estudios)
  )

desempleo_estudios |>
  ggplot(aes(x = anno, porcentaje, color = grupo_de_edad)) +
  geom_point() +
  geom_line() +
  facet_wrap(~nivel_de_formacion_alcanzado) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 5))

### Etiquetar puntos ---------------------------------------------------------------------

bigmac <- read_csv("datos/precio_bigmac_por_pais.csv")

bigmac |>
  ggplot(aes(x = dollar_ex, y = dollar_price)) +
  geom_point(alpha = 0.2)

library(ggrepel)

bigmac |>
  filter(dollar_ex < 10) |>
  ggplot(aes(x = dollar_ex, y = dollar_price)) +
  geom_point(alpha = 0.2) +
  geom_text_repel(aes(label = name), size = 3) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 10, 0.5))

### Entrada de datos desde varias fuentes --------------------------------------------

View(economics)
?economics

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line()

ggplot(economics, aes(x = date, y = unemploy / pop)) +
  geom_line()

# El siguiente codigo define un tibble con los periodos de recesion
# en EEUU desde 1969
periodos_de_recesion <- tibble(
  inicio = as.Date(c(
    "1969-12-01", "1973-11-01",
    "1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01"
  )),
  fin = as.Date(c(
    "1970-11-01", "1975-03-01",
    "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01"
  ))
)

ggplot(economics) +
  geom_line(aes(x = date, y = unemploy / pop)) +
  geom_rect(
    data = periodos_de_recesion,
    aes(xmin = inicio, xmax = fin), ymin = -Inf, ymax = +Inf,
    fill = "red", alpha = 0.2
  )


### EJERCICIO Bigmac Europa y USA -------------------------------

# NO HAGAS TRAMPA !!!!!!!!!!!!!!!!!!!!!!!!

# bigmac_usa_y_europa <- bigmac |>
#   filter(name %in% c("United States", "Euro area"))
#
# bigmac |>
#   filter(dollar_ex < 10) |>
#   ggplot(aes(x = dollar_ex, y = dollar_price)) +
#   geom_point(alpha = 0.2) +
#   geom_point(data = bigmac_usa_y_europa, shape = "circle open", size = 3, color = "red") +
#   geom_text_repel(data = bigmac_usa_y_europa, aes(label = name), size = 3) +
#   scale_x_continuous(breaks = seq(0, 10, 1)) +
#   scale_y_continuous(breaks = seq(0, 10, 0.5))
#
# ggsave("bigmac.pdf", width = 3, height = 3)

## 2 variables categóricas ---------------------------------------------------

with(
  penguins,
  table(species, island)
)

penguins |>
  ggplot(aes(x = species, fill = island)) +
  geom_bar()

with(
  penguins,
  table(island, species)
)
penguins |>
  ggplot(aes(x = island, fill = species)) +
  geom_bar()

penguins |>
  ggplot(aes(x = island, fill = species)) +
  geom_bar(position = "fill")

penguins |>
  ggplot(aes(x = island, fill = species)) +
  geom_bar(position = "dodge")

with(
  penguins,
  table(species, island)
)
penguins |>
  count(island, species)

penguins |>
  count(island, species) |>
  ggplot(aes(x = island, y = species)) +
  geom_tile(aes(fill = n))

### Pivot longer ---------------------------------------------------------

bebidas <- read_csv("datos/drinks_by_country.csv")

df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)
df

df |>
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

bebidas <- bebidas |>
  pivot_longer(
    cols = c("beer_servings", "spirit_servings", "wine_servings"),
    names_to = "tipo_bebida",
    values_to = "cantidad"
  )

# Ver https://ggplot2-book.org/scales-colour#sec-colour-continuous

bebidas |>
  filter(continent == "Europe") |>
  ggplot(aes(y = country, x = tipo_bebida)) +
  geom_tile(aes(fill = cantidad)) +
  scale_fill_viridis_c(option = "magma") +
  scale_y_discrete(limits = rev)

# Mejorando el aspecto visual de las gráficas -----------------------------

## Etiquetas (ejes,títulos y títulos de leyendas) ----------------------------

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm) +
  labs(
    x = "Año de la película",
    y = "Nº de muertes",
    title = "Muertes causadas por James\nBond a lo largo de los años",
    subtitle = "¿Ha aumentado la violencia\nen las películas de James Bond?",
  )

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm) +
  labs(
    x = "Año de la película",
    y = "Nº de muertes",
    title = str_wrap("Muertes causadas por James\nBond a lo largo de los años",
      width = 40
    ),
    subtitle = str_wrap("¿Ha aumentado la violencia\nen las películas de James Bond?",
      width = 50
    ),
  )

james_bond |>
  ggplot(aes(y = fct_infreq(Bond))) +
  geom_bar(aes(fill = Bond, color = Bond)) +
  labs(
    x = "Número de películas",
    y = "Actor que interpreta a James Bond",
    fill = "Actor",
    color = "Actor"
  )

james_bond |>
  ggplot(aes(y = fct_infreq(Bond))) +
  geom_bar(aes(fill = Bond, color = Bond)) +
  labs(
    x = "Número de películas",
    y = "Actor que interpreta a James Bond",
  ) +
  theme(legend.position = "none")

## Escalas de los ejes y las leyendas ----------------------------

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm) +
  labs(
    x = "Año de la película",
    y = "Nº de muertes",
    title = str_wrap("Muertes causadas por James\nBond a lo largo de los años",
      width = 40
    ),
    subtitle = str_wrap("¿Ha aumentado la violencia\nen las películas de James Bond?",
      width = 50
    ),
  ) +
  scale_x_continuous(breaks = seq(1960, 2025, 4)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

parejas |>
  ggplot(aes(y = numero_de_parejas, x = sexo)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_x_discrete(labels = c("Masculino", "Femenino")) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  labs(
    x = "Sexo",
    y = "Número de parejas"
  )

desempleo_estudios |>
  mutate(
    nivel_de_formacion_alcanzado =
      fct_recode(nivel_de_formacion_alcanzado,
        "< Primaria" = "Menos quthemee primaria",
        "Primaria - 1ª etapa sec" = "Ed. primaria y primera etapa de Ed. secundaria",
        "2ª etapa sec - postsec no superior" = "Segunda etapa de Ed. secundaria y Ed. postsecundaria no superior",
        "Superior" = "Ed. superior"
      )
  ) |>
  ggplot(aes(x = anno, porcentaje, color = grupo_de_edad)) +
  geom_point() +
  geom_line() +
  facet_wrap(~nivel_de_formacion_alcanzado) +
  scale_color_discrete(
    labels = c("16-24 años", "25-54 años", "55-64 años"),
  ) +
  labs(
    x = "Año",
    y = "Porcentaje de desempleo",
    color = "Grupo de edad",
    title = str_wrap(
      "Desempleo por grupo de edad y nivel de formación alcanzado",
      30
    )
  ) +
  theme(legend.position = "left") +
  scale_color_brewer(palette = "Dark2")

## Temas -----------------------------------------------------------------

james_bond |>
  ggplot(aes(x = Year, y = Kills_Bond)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm) +
  labs(
    x = "Año de la película",
    y = "Nº de muertes",
    title = str_wrap("Muertes causadas por James\nBond a lo largo de los años",
      width = 40
    ),
    subtitle = str_wrap("¿Ha aumentado la violencia\nen las películas de James Bond?",
      width = 50
    ),
  ) +
  theme_minimal()
