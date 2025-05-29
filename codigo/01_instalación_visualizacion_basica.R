install.packages("tidyverse", "ggrepel", "palmerpenguins")

library(tidyverse)

# EDA - VARIACIÓN ---------------------------------------------------------------

## 1 Variable numérica --------------------------------------------------

parejas <- read_csv("datos/numero_de_parejas_por_sexo.csv")

parejas |> 
  filter(sexo == "Hombre") |> 
  ggplot(aes(x = numero_de_parejas)) +
  geom_histogram()

# Cambia el nº de bars. Prueba con binwidth o con bins  

parejas |> 
  filter(sexo == "Hombre") |> 
  ggplot(aes(x = numero_de_parejas)) +
  geom_density()

parejas |> 
  filter(sexo == "Hombre") |> 
  ggplot(aes(x = numero_de_parejas)) +
  geom_density() +
  coord_cartesian(xlim = c(0, 30)) +
  scale_x_continuous(breaks = seq(0, 30, 2))

## 1 variable categórica ---------------------------------------------------

### James Bond --------------------------------------------------------------

james_bond <- read_csv("datos/james_bond.csv")

james_bond |> 
  ggplot(aes(x = Bond)) +
  geom_bar()

james_bond |> 
  ggplot(aes(y = Bond)) +
  geom_bar()

james_bond |> 
  ggplot(aes(y = fct_infreq(Bond))) +
  geom_bar(aes(fill = Bond), color = "black") 

james_bond |> 
  ggplot(aes(y = factor(1), fill = Bond)) +
  geom_bar(aes(fill = Bond), width = 1, color = "black") +
  coord_polar() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank())

### Estaciones Tren (Media y Larga Distancia) -------------------------------

estaciones_tren <- read_csv("datos/estaciones_tren_larga_y_media_distancia.csv")

estaciones_tren |> 
  ggplot(aes(y = fct_infreq(provincia))) +
  geom_bar() 

## Valores atípicos (outliers) ---------------------------------------------

View(diamonds)

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) 

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

diamonds |> 
  filter(y < 3 | y > 20) 

# Un diamante no puede:
#   * medir 0 mm
#   * ser gigante y sólo costar 2075 dólares

# Opción 1: eliminamos la fila entera
# Peligro: nos podemos quedar sin datos!

diamonds_limipieza_drastica <- diamonds |> 
  filter((y >= 3) & (y < 20 | price > 12000)) 

diamonds_limipieza_drastica |> 
  filter(y < 3 | y > 20) 

# Opción 2: ponemos los valores raros a NA

diamonds_limipieza_menos_drastica <- diamonds |> 
  mutate(y = if_else((y < 3) | (y > 20 & price <= 12000), NA, y)) 

ggplot(diamonds_limipieza_menos_drastica, aes(x = y)) +
  geom_histogram(binwidth = 0.5) 

# ggplot nos avisa de que hay NAs

# EDA - COVARIACIÓN -------------------------------------------------------------

## 1 Variable numérica según una categórica -----------------------------------

parejas |> 
  ggplot(aes(x = numero_de_parejas, color = sexo)) +
  geom_density(linewidth = 1, alpha = 0.2)

parejas |> 
  ggplot(aes(x = numero_de_parejas, y = sexo)) +
  geom_boxplot()

parejas |> 
  ggplot(aes(y = numero_de_parejas, x = sexo)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 50))


## 2 variables numericas ---------------------------------------------------

### James Bond --------------------------------------------------------------

james_bond |> 
  ggplot(aes(x = Year, y = Martinis, color = Bond, shape = Bond)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

james_bond |> 
  ggplot(aes(x = Budget, y = Avg_User_IMDB)) +
  geom_point() 

james_bond |> 
  ggplot(aes(x = Budget, y = Avg_User_IMDB)) +
  geom_point(aes(color = Year))

james_bond |> 
  mutate(last_25_years = if_else(Year < 2000, FALSE, TRUE)) |> 
  ggplot(aes(x = Budget, y = Avg_User_IMDB)) +
  geom_point(aes(color = last_25_years)) 
  
james_bond |> 
  mutate(last_25_years = if_else(Year < 2000, FALSE, TRUE)) |> 
  ggplot(aes(x = Budget, y = Avg_User_IMDB, color = last_25_years)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")  

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
  ggplot(aes(x = anno, porcentaje, color = grupo_de_edad))+
  geom_point() +
  geom_line() +
  facet_wrap(~nivel_de_formacion_alcanzado) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 5))

niveles_estudios <- c(
    "Menos que primaria (nivel 0)",
    "Educación primaria y primera etapa de educación secundaria  (niveles 1 y 2)",
    "Segunda etapa de educación secundaria y educación postsecundaria no superior (niveles 3 y 4)",
    "Educación superior (terciaria)(niveles 5 a 8)"
)

desempleo_estudios <- desempleo_estudios |> 
  mutate(nivel_de_formacion_alcanzado = 
           factor(nivel_de_formacion_alcanzado, levels = niveles_estudios))

desempleo_estudios |> 
  ggplot(aes(x = anno, porcentaje, color = grupo_de_edad))+
  geom_point() +
  geom_line() +
  facet_wrap(~nivel_de_formacion_alcanzado) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 5))

### Overplotting ------------------------------------------------------------

library(palmerpenguins)

penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

bigmac <- read_csv("datos/precio_bigmac_por_pais.csv")

bigmac |> 
  ggplot(aes(x = dollar_ex, y = dollar_price)) +
  geom_point() 

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

ggplot(economics, aes(x = date , y = unemploy)) +
  geom_line()

ggplot(economics, aes(x = date , y = unemploy/pop)) +
  geom_line()

# El siguiente codigo define un tibble con los periodos de recesion
# en EEUU desde 1969
periodos_de_recesion <-  tibble(
  inicio = as.Date(c("1969-12-01", "1973-11-01",
                          "1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01")),
  fin = as.Date ( c("1970-11-01", "1975-03-01",
                      "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01"))
)

ggplot(economics) +
  geom_line(aes(x = date , y = unemploy/pop)) +
  geom_rect(data=periodos_de_recesion,
            aes(xmin=inicio, xmax=fin) , ymin=-Inf , ymax=+Inf,
              fill = "red", alpha=0.2)

## 2 variables categóricas ---------------------------------------------------

with(penguins,
  table(species, island)
)

penguins |> 
  ggplot(aes(x = species, fill = island)) +
  geom_bar()

with(penguins,
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


with(penguins,
     table(species, island)
)
penguins |> 
  count(island, species)

penguins |> 
  count(island, species) |> 
  ggplot(aes(x = island, y = species)) +
  geom_tile(aes(fill = n))

# ORGANIZAR DATOS ---------------------------------------------------------

bebidas <- read_csv("datos/drinks_by_country.csv")

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

# COMUNICAR ---------------------------------------------------------------


